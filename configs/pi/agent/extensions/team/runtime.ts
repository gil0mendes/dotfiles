import type {
	EventBus,
	ExtensionAPI,
	ModelRegistry,
} from "@earendil-works/pi-coding-agent";
import {
	SubagentSessionRunner,
	type BootstrapContext,
	type SubagentRunner,
} from "./subagentRunner";
import type {
	PendingMessage,
	SettledSubagentStatus,
	SteeringPayload,
	SubagentState,
	SubagentStatus,
} from "./types";
import type { AgentConfig } from "./loader/types";
import type { Api, Model } from "@earendil-works/pi-ai";
import { nanoid } from "nanoid";
import {
	emitTeamRuntimeChanged,
	emitTeamWarning,
	type TeamRuntimeChangeReason,
} from "./events";
import { getSteeringMessagePayload } from "./message/steeringPayload";

const PENDING_MESSAGE_TTL_MS = 86_400_000; // 24 hours

export type ActiveRuntimeBinding = {
	/**
	 * Current working directory.
	 */
	cwd: string;
	sessionId: string;
	sendMessage: ExtensionAPI["sendMessage"];
	isIdle: () => boolean;
	events: EventBus;
};

type SpawnContext = {
	model: Model<Api> | undefined;
	modelRegistry: ModelRegistry;
	agentDir: string;
	parentSessionFile?: string;
	availableToolNames: readonly string[];
	brief?: string;
	onWarning?: (message: string) => void;
};

export type ActiveAgentSummary = {
	id: string;
	agentName: string;
	status: SubagentStatus;
	turns: number;
	contextTokens: number;
	model: string | undefined;
};

type AbortOptions = {
	reason: string;
};

const isAbortableStatus = (status: SubagentStatus): boolean => {
	return status === "running" || status === "waiting";
};

const buildActiveAgentSummary = (state: SubagentState): ActiveAgentSummary => {
	return {
		id: state.id,
		agentName: state.agentConfig.name,
		status: state.status,
		turns: state.turns,
		contextTokens: state.contextTokens,
		model: state.model,
	};
};

/** Generates a unique agent ID based on the agent name and a random nanoid. */
const generateAgentId = (agentName: string): string =>
	`${agentName}-${nanoid(8)}`;

const createAgent = (
	agentConfig: AgentConfig,
	task: string,
	brief: string,
	ownerSessionId: string,
): SubagentState => {
	const id = generateAgentId(agentConfig.name);
	const state: SubagentState = {
		id,
		agentConfig,
		task,
		brief,
		status: "running",
		ownerSessionId,
		session: null,
		turns: 0,
		contextTokens: 0,
	};

	return state;
};

const toBootstrapContext = (ctx: SpawnContext): BootstrapContext => ({
	model: ctx.model,
	modelRegistry: ctx.modelRegistry,
	agentDir: ctx.agentDir,
	parentSessionFile: ctx.parentSessionFile,
	availableToolNames: ctx.availableToolNames,
});

export class Runtime {
	private readonly agents = new Map<string, SubagentState>();
	private readonly runner: SubagentRunner;
	private readonly scheduleFlush: (callback: () => void) => void;
	private readonly now = Date.now;
	private activeBinding?: ActiveRuntimeBinding;
	private pendingMessages: PendingMessage[] = [];
	private flushScheduled = false;

	constructor() {
		// TODO: move this logic outside to make it more modular
		this.scheduleFlush = (fn) => setTimeout(fn, 0);
		this.runner = new SubagentSessionRunner({
			isCurrent: (state) => this.agents.get(state.id) === state,
			onProgress: (ownerSessionId) => {
				this.notifyRuntimeChanged(ownerSessionId, "progressed");
			},
			onSettled: (
				state: SubagentState,
				status: SettledSubagentStatus,
				outcome: { result?: string; error?: string },
			) => {
				this.settleAgent(state, status, outcome);
			},
		});
	}

	abort(id: string, opts: AbortOptions): boolean {
		const state = this.agents.get(id);
		if (!state || !isAbortableStatus(state.status)) {
			return false;
		}

		this.runner.abort(state);
		this.settleAgent(state, "aborted", { error: opts.reason });
		return true;
	}

	abortAll() {
		const allAgents = Array.from(this.agents.values()).filter((state) =>
			isAbortableStatus(state.status),
		);
		for (const state of allAgents) {
			this.abort(state.id, { reason: "Aborted during shutdown" });
		}
	}

	abortAllOwnedBy(ownerSessionId: string, opts: AbortOptions) {
		const ids = Array.from(this.agents.values())
			.filter(
				(state) =>
					state.ownerSessionId === ownerSessionId &&
					isAbortableStatus(state.status),
			)
			.map((state) => state.id);

		for (const id of ids) {
			this.abort(id, opts);
		}

		return ids;
	}

	activateSession(binding: ActiveRuntimeBinding): void {
		this.activeBinding = binding;
		this.notifyRuntimeChanged(binding.sessionId, "activated");
		this.schedulePendingFlushFor(binding.sessionId);
	}

	terminateSession(sessionId: string): void {
		if (this.activeBinding?.sessionId === sessionId) {
			this.activeBinding = undefined;
		}
	}

	reportWarning(ownerSessionId: string, message: string): void {
		if (this.activeBinding?.sessionId !== ownerSessionId) return;
		emitTeamWarning(this.activeBinding.events, { ownerSessionId, message });
	}

	spawn(
		subagent: AgentConfig,
		task: string,
		cwd: string,
		ownerSessionId: string,
		ctx: SpawnContext,
	): string {
		const agentState = createAgent(
			subagent,
			task,
			ctx.brief ?? "",
			ownerSessionId,
		);
		this.agents.set(agentState.id, agentState);
		this.notifyRuntimeChanged(ownerSessionId, "spawned", agentState.id);

		this.runner.start(agentState, {
			cwd,
			ctx: toBootstrapContext(ctx),
			onWarning: ctx.onWarning,
		});

		return agentState.id;
	}

	respond(
		subagentId: string,
		message: string,
		ownerSessionId: string,
	): { error?: string } {
		const transition = this.startSubagentResponse(subagentId, ownerSessionId);
		if (transition.ok === false) {
			return { error: transition.error };
		}

		this.notifyRuntimeChanged(ownerSessionId, "responded", subagentId);
		this.runner.respond(transition.state, message);
		return {};
	}

	done(id: string, callerSessionId: string): { error?: string } {
		const transition = this.validateSubagentDone(id, callerSessionId);
		if (!transition.ok) {
			return { error: transition.error };
		}

		this.disposeAgent(transition.state);
		return {};
	}

	getActiveSummariesForOwner(ownerSessionId: string): ActiveAgentSummary[] {
		return Array.from(this.agents.values())
			.filter(
				(state) =>
					isAbortableStatus(state.status) &&
					state.ownerSessionId === ownerSessionId,
			)
			.map(buildActiveAgentSummary);
	}

	getCwd(): string {
		return this.activeBinding?.cwd ?? "";
	}

	private validateOwnedSubagent(
		id: string,
		callerSessionId: string,
		missingMessage: string,
	): { ok: true; state: SubagentState } | { ok: false; error: string } {
		const state = this.agents.get(id);
		if (!state) {
			return { ok: false, error: missingMessage };
		}
		if (state.ownerSessionId !== callerSessionId) {
			return {
				ok: false,
				error: `Subagent "${id}" belongs to a different session`,
			};
		}
		return { ok: true, state };
	}

	private validateSubagentDone(
		id: string,
		callerSessionId: string,
	): { ok: true; state: SubagentState } | { ok: false; error: string } {
		const owned = this.validateOwnedSubagent(
			id,
			callerSessionId,
			`No active subagent with id "${id}"`,
		);

		if (!owned.ok) {
			return owned;
		}

		if (owned.state.status !== "waiting") {
			return { ok: false, error: `Subagent "${id}" is not in waiting state` };
		}

		return owned;
	}

	private startSubagentResponse(
		id: string,
		callerSessionId: string,
	): { ok: true; state: SubagentState } | { ok: false; error: string } {
		const owned = this.validateOwnedSubagent(
			id,
			callerSessionId,
			`No subagent with id "${id}"`,
		);
		if (!owned.ok) {
			return owned;
		}
		if (owned.state.status !== "waiting") {
			return {
				ok: false,
				error: `Subagent "${id}" is not waiting for a response (status: ${owned.state.status})`,
			};
		}
		if (!owned.state.session) {
			return { ok: false, error: `Subagent "${id}" has no active session` };
		}

		owned.state.status = "running";
		return owned;
	}

	private schedulePendingFlushFor(sessionId: string): void {
		// when no pending messages for this session, no need to schedule a flush
		const hasMessagesForThisSession = this.pendingMessages.some(
			(entry) => entry.ownerSessionId === sessionId,
		);
		if (!hasMessagesForThisSession) {
			return;
		}

		this.flushScheduled = true;
		this.scheduleFlush(() => {
			this.flushScheduled = false;
			this.flushPending();
		});
	}

	private cleanStaleMessages(): void {
		const cutoffTime = this.now() - PENDING_MESSAGE_TTL_MS;
		this.pendingMessages = this.pendingMessages.filter(
			(entry) => entry.queuedAt >= cutoffTime,
		);
	}

	private flushPending(): void {
		if (!this.activeBinding) {
			return;
		}

		const targetSessionId = this.activeBinding.sessionId;
		this.cleanStaleMessages();

		const toDeliver: PendingMessage[] = [];
		const remaining: PendingMessage[] = [];
		for (const entry of this.pendingMessages) {
			if (entry.ownerSessionId === targetSessionId) {
				toDeliver.push(entry);
			} else {
				remaining.push(entry);
			}
		}
		this.pendingMessages = remaining;

		for (const entry of toDeliver) {
			this.send(entry.ownerSessionId, entry.payload);
		}
	}

	private queue(ownerSessionId: string, payload: SteeringPayload): void {
		this.pendingMessages.push({
			ownerSessionId,
			payload,
			queuedAt: this.now(),
		});
	}

	private countRunningForOwner(
		ownerSessionId: string,
		excludeId: string,
	): number {
		let count = 0;
		for (const state of this.agents.values()) {
			if (
				state.id !== excludeId &&
				state.ownerSessionId === ownerSessionId &&
				state.status === "running"
			)
				count++;
		}
		return count;
	}

	private deliver(ownerSessionId: string, payload: SteeringPayload): void {
		if (
			!this.activeBinding ||
			ownerSessionId !== this.activeBinding.sessionId ||
			this.flushScheduled
		) {
			this.queue(ownerSessionId, payload);
			return;
		}
		this.send(ownerSessionId, payload);
	}

	private send(ownerSessionId: string, payload: SteeringPayload): void {
		if (
			!this.activeBinding ||
			this.activeBinding.sessionId !== ownerSessionId
		) {
			this.queue(ownerSessionId, payload);
			return;
		}

		const remaining = this.countRunningForOwner(ownerSessionId, payload.id);
		const isIdle = this.activeBinding.isIdle();
		const triggerResultTurn =
			payload.status === "waiting" || !(isIdle && remaining > 0);

		const steeringPayload = getSteeringMessagePayload(payload, {
			isIdle,
			triggerTurn: triggerResultTurn,
		});

		this.activeBinding.sendMessage(
			steeringPayload.message,
			steeringPayload.options,
		);
	}

	private notifyRuntimeChanged(
		ownerSessionId: string,
		reason: TeamRuntimeChangeReason,
		agentId?: string,
	): void {
		if (this.activeBinding?.sessionId !== ownerSessionId) return;
		emitTeamRuntimeChanged(this.activeBinding.events, {
			ownerSessionId,
			reason,
			agentId,
		});
	}

	private settleAgent(
		state: SubagentState,
		nextStatus: SettledSubagentStatus,
		opts: { result?: string; error?: string },
	): void {
		if (this.agents.get(state.id) !== state) return;

		state.status = nextStatus;
		state.result = opts.result;
		state.error = opts.error;
		this.notifyRuntimeChanged(state.ownerSessionId, "settled", state.id);

		this.deliver(state.ownerSessionId, {
			id: state.id,
			agentName: state.agentConfig.name,
			sessionFile: state.session?.sessionFile,
			status: state.status,
			result: state.result,
			error: state.error,
		});

		if (state.status !== "waiting") {
			this.disposeAgent(state);
		}
	}

	private disposeAgent(state: SubagentState): void {
		state.unsubscribe?.();
		state.session?.dispose();
		this.agents.delete(state.id);
		this.notifyRuntimeChanged(state.ownerSessionId, "removed", state.id);
	}
}

const runtimeKey = Symbol.for("team.runtime");
const globalWithRuntime = globalThis as unknown as { [runtimeKey]: Runtime };

export const team = (globalWithRuntime[runtimeKey] ??= new Runtime());
