import type { EventBus, ExtensionAPI, ModelRegistry } from "@earendil-works/pi-coding-agent";
import {
	SubagentSessionRunner,
	type BootstrapContext,
	type SubagentRunner,
	type SubagentRunnerCallbacks,
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
	emitTeamConversationChanged,
	isTeamConversationChangedPayload,
	TEAM_CONVERSATION_CHANGED_EVENT,
	type TeamRuntimeChangeReason,
} from "./events";
import {
	Conversation,
	boundedText,
	CONVERSATION_RECENT_TTL_MS,
	CONVERSATION_RECENT_PER_OWNER,
	CONVERSATION_RECENT_MAX_COUNT,
	CONVERSATION_RECENT_MAX_BYTES,
	type ConversationSnapshot,
} from "./conversation";
import { getSteeringMessagePayload } from "./message/steeringPayload";

const PENDING_MESSAGE_TTL_MS = 86_400_000; // 24 hours
export const RECENT_AGENT_TTL_MS = CONVERSATION_RECENT_TTL_MS;

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
	brief: string;
	status: SubagentStatus;
	turns: number;
	contextTokens: number;
	model: string | undefined;
	startedAt: number;
	result: string | undefined;
	error: string | undefined;
};

export type RecentAgentSummary = ActiveAgentSummary & {
	settledAt: number;
};

export type FleetSnapshot = {
	active: ActiveAgentSummary[];
	recent: RecentAgentSummary[];
	detachedActiveCount: number;
};

type AbortOptions = {
	reason: string;
};

type RuntimeOptions = {
	createRunner?: (callbacks: SubagentRunnerCallbacks) => SubagentRunner;
	now?: () => number;
};

const isAbortableStatus = (status: SubagentStatus): boolean => {
	return status === "running" || status === "waiting";
};

const buildActiveAgentSummary = (state: SubagentState): ActiveAgentSummary => {
	return {
		id: state.id,
		agentName: state.agentConfig.name,
		brief: state.brief,
		status: state.status,
		turns: state.turns,
		contextTokens: state.contextTokens,
		model: state.model,
		startedAt: state.startedAt,
		result: state.result,
		error: state.error,
	};
};

/** Generates a unique agent ID based on the agent name and a random nanoid. */
const generateAgentId = (agentName: string): string => `${agentName}-${nanoid(8)}`;

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
		startedAt: Date.now(),
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
	private readonly now: () => number;
	private readonly bindings = new Map<string, ActiveRuntimeBinding>();
	private readonly recentByOwner = new Map<string, RecentAgentSummary[]>();
	private readonly conversations = new Map<string, Conversation>();
	private readonly recentConversations = new Map<
		string,
		{ owner: string; conversation: Conversation; summary: RecentAgentSummary; size: number }
	>();
	private expiryTimer: ReturnType<typeof setTimeout> | undefined;
	private readonly conversationSubscriptions = new Map<string, number>();
	private readonly conversationUnsubscribers = new Map<string, Set<() => void>>();
	private readonly conversationNotifications = new Map<string, ReturnType<typeof setTimeout>>();
	private readonly sessionAgents = new Map<string, AgentConfig>();
	private readonly scheduledFlushes = new Set<string>();
	private pendingMessages: PendingMessage[] = [];

	constructor(options: RuntimeOptions = {}) {
		// TODO: move this logic outside to make it more modular
		this.scheduleFlush = (fn) => setTimeout(fn, 0);
		this.now = options.now ?? Date.now;
		const callbacks: SubagentRunnerCallbacks = {
			isCurrent: (state) => this.agents.get(state.id) === state,
			onSessionAttached: (state, sessionId) => {
				if (this.agents.get(state.id) !== state) return;
				this.sessionAgents.set(sessionId, state.agentConfig);
				this.observe(state.ownerSessionId, () => {
					this.conversations.get(state.id)?.touch();
					this.notifyConversation(state.ownerSessionId, state.id);
				});
				this.notifyRuntimeChanged(state.ownerSessionId, "model_updated", state.id);
			},
			onConversationEvent: (state, event) => {
				if (this.agents.get(state.id) !== state || state.status !== "running") return;
				this.observe(state.ownerSessionId, () => {
					if (this.conversations.get(state.id)?.capture(event))
						this.notifyConversation(state.ownerSessionId, state.id);
				});
			},
			onConversationError: (state) => {
				try {
					this.reportWarning(
						state.ownerSessionId,
						"Team conversation capture failed; agent execution continues.",
					);
				} catch {
					/* Observability must not interrupt execution. */
				}
			},
			onProgress: (state) => {
				if (this.agents.get(state.id) !== state) return;
				this.observe(state.ownerSessionId, () => {
					this.conversations.get(state.id)?.touch();
					this.notifyConversation(state.ownerSessionId, state.id);
				});
				this.notifyRuntimeChanged(state.ownerSessionId, "progressed");
			},
			onSettled: (
				state: SubagentState,
				status: SettledSubagentStatus,
				outcome: { result?: string; error?: string },
			) => {
				this.settleAgent(state, status, outcome);
			},
		};
		this.runner = options.createRunner?.(callbacks) ?? new SubagentSessionRunner(callbacks);
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

	abortOwned(id: string, ownerSessionId: string, opts: AbortOptions): boolean {
		const state = this.agents.get(id);
		if (!state || state.ownerSessionId !== ownerSessionId) {
			return false;
		}
		return this.abort(id, opts);
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
			.filter((state) => state.ownerSessionId === ownerSessionId && isAbortableStatus(state.status))
			.map((state) => state.id);

		for (const id of ids) {
			this.abort(id, opts);
		}

		return ids;
	}

	activateSession(binding: ActiveRuntimeBinding): void {
		if (this.bindings.get(binding.sessionId) !== binding)
			this.clearConversationSubscriptions(binding.sessionId);
		this.bindings.set(binding.sessionId, binding);
		this.notifyRuntimeChanged(binding.sessionId, "activated");
		this.schedulePendingFlushFor(binding.sessionId);
	}

	terminateSession(sessionId: string): void {
		this.clearConversationSubscriptions(sessionId);
		this.bindings.delete(sessionId);
		this.sessionAgents.delete(sessionId);
		for (const [id, entry] of this.recentConversations) {
			if (entry.owner === sessionId) this.removeRecent(id);
		}
		this.scheduleExpiry();
	}

	reportWarning(ownerSessionId: string, message: string): void {
		const binding = this.bindings.get(ownerSessionId);
		if (!binding) return;
		emitTeamWarning(binding.events, { ownerSessionId, message });
	}

	getSessionAgent(sessionId: string): AgentConfig | undefined {
		return this.sessionAgents.get(sessionId);
	}

	spawn(
		subagent: AgentConfig,
		task: string,
		cwd: string,
		ownerSessionId: string,
		ctx: SpawnContext,
	): string {
		const agentState = createAgent(subagent, task, ctx.brief ?? "", ownerSessionId);
		this.agents.set(agentState.id, agentState);
		this.observe(ownerSessionId, () => {
			const conversation = new Conversation();
			conversation.prompt(task);
			this.conversations.set(agentState.id, conversation);
		});
		this.notifyRuntimeChanged(ownerSessionId, "spawned", agentState.id);

		void this.runner
			.start(agentState, {
				cwd,
				ctx: toBootstrapContext(ctx),
				onWarning: ctx.onWarning,
			})
			.catch((error: unknown) => {
				const message = error instanceof Error ? error.message : String(error);
				this.settleAgent(agentState, "error", { error: message });
			});

		return agentState.id;
	}

	respond(subagentId: string, message: string, ownerSessionId: string): { error?: string } {
		const transition = this.startSubagentResponse(subagentId, ownerSessionId);
		if (transition.ok === false) {
			return { error: transition.error };
		}

		this.observe(ownerSessionId, () => {
			this.conversations.get(subagentId)?.prompt(message);
			this.notifyConversation(ownerSessionId, subagentId);
		});
		this.notifyRuntimeChanged(ownerSessionId, "responded", subagentId);
		this.runner.respond(transition.state, message);
		return {};
	}

	done(id: string, callerSessionId: string): { error?: string } {
		const transition = this.validateSubagentDone(id, callerSessionId);
		if (!transition.ok) {
			return { error: transition.error };
		}

		transition.state.status = "done";
		this.observe(transition.state.ownerSessionId, () => {
			this.conversations.get(id)?.touch();
			this.rememberSettledAgent(transition.state);
			this.notifyConversation(transition.state.ownerSessionId, id);
		});
		this.disposeAgent(transition.state);
		return {};
	}

	getConversationForOwner(
		ownerSessionId: string,
		agentId: string,
	): ConversationSnapshot | undefined {
		const state = this.agents.get(agentId);
		if (state?.ownerSessionId === ownerSessionId)
			return this.conversations.get(agentId)?.snapshot(buildActiveAgentSummary(state));
		this.cleanRecent();
		const recent = this.recentConversations.get(agentId);
		if (!recent || recent.owner !== ownerSessionId) return undefined;
		return recent.conversation.snapshot(
			recent.summary,
			recent.summary.settledAt + RECENT_AGENT_TTL_MS,
		);
	}

	/** Subscribe through this method so streaming notifications are scheduled only for viewers. */
	subscribeConversation(ownerSessionId: string, agentId: string, listener: () => void): () => void {
		const binding = this.bindings.get(ownerSessionId);
		if (!binding || !this.getConversationForOwner(ownerSessionId, agentId)) return () => {};
		const count = this.conversationSubscriptions.get(agentId) ?? 0;
		this.conversationSubscriptions.set(agentId, count + 1);
		const unsubscribe = binding.events.on(TEAM_CONVERSATION_CHANGED_EVENT, (payload) => {
			if (
				isTeamConversationChangedPayload(payload) &&
				payload.ownerSessionId === ownerSessionId &&
				payload.agentId === agentId
			)
				listener();
		});
		let active = true;
		const subscriptions =
			this.conversationUnsubscribers.get(ownerSessionId) ?? new Set<() => void>();
		this.conversationUnsubscribers.set(ownerSessionId, subscriptions);
		const close = () => {
			if (!active) return;
			active = false;
			unsubscribe();
			subscriptions.delete(close);
			if (!subscriptions.size) this.conversationUnsubscribers.delete(ownerSessionId);
			const remaining = (this.conversationSubscriptions.get(agentId) ?? 1) - 1;
			if (remaining > 0) this.conversationSubscriptions.set(agentId, remaining);
			else {
				this.conversationSubscriptions.delete(agentId);
				const timer = this.conversationNotifications.get(agentId);
				if (timer) clearTimeout(timer);
				this.conversationNotifications.delete(agentId);
			}
		};
		subscriptions.add(close);
		return close;
	}

	getActiveSummariesForOwner(ownerSessionId: string): ActiveAgentSummary[] {
		return Array.from(this.agents.values())
			.filter((state) => isAbortableStatus(state.status) && state.ownerSessionId === ownerSessionId)
			.map(buildActiveAgentSummary);
	}

	getFleetSnapshotForOwner(ownerSessionId: string): FleetSnapshot {
		this.cleanRecent();
		return {
			active: this.getActiveSummariesForOwner(ownerSessionId),
			recent: [...(this.recentByOwner.get(ownerSessionId) ?? [])],
			detachedActiveCount: this.getDetachedActiveCount(ownerSessionId),
		};
	}

	getDetachedActiveCount(ownerSessionId: string): number {
		let count = 0;
		for (const state of this.agents.values()) {
			if (isAbortableStatus(state.status) && state.ownerSessionId !== ownerSessionId) count++;
		}
		return count;
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
		const owned = this.validateOwnedSubagent(id, callerSessionId, `No subagent with id "${id}"`);
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
		const hasMessagesForThisSession = this.pendingMessages.some(
			(entry) => entry.ownerSessionId === sessionId,
		);
		if (!hasMessagesForThisSession || this.scheduledFlushes.has(sessionId)) {
			return;
		}

		this.scheduledFlushes.add(sessionId);
		this.scheduleFlush(() => {
			this.scheduledFlushes.delete(sessionId);
			this.flushPendingFor(sessionId);
		});
	}

	private cleanStaleMessages(): void {
		const cutoffTime = this.now() - PENDING_MESSAGE_TTL_MS;
		this.pendingMessages = this.pendingMessages.filter((entry) => entry.queuedAt >= cutoffTime);
	}

	private flushPendingFor(sessionId: string): void {
		if (!this.bindings.has(sessionId)) return;

		this.cleanStaleMessages();
		const toDeliver: PendingMessage[] = [];
		const remaining: PendingMessage[] = [];
		for (const entry of this.pendingMessages) {
			if (entry.ownerSessionId === sessionId) {
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

	private countRunningForOwner(ownerSessionId: string, excludeId: string): number {
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
		if (!this.bindings.has(ownerSessionId)) {
			this.queue(ownerSessionId, payload);
			return;
		}
		this.send(ownerSessionId, payload);
	}

	private send(ownerSessionId: string, payload: SteeringPayload): void {
		const binding = this.bindings.get(ownerSessionId);
		if (!binding) {
			this.queue(ownerSessionId, payload);
			return;
		}

		const remaining = this.countRunningForOwner(ownerSessionId, payload.id);
		const isIdle = binding.isIdle();
		const triggerResultTurn = payload.status === "waiting" || !(isIdle && remaining > 0);

		const steeringPayload = getSteeringMessagePayload(payload, {
			isIdle,
			triggerTurn: triggerResultTurn,
		});

		binding.sendMessage(steeringPayload.message, steeringPayload.options);
	}

	private notifyRuntimeChanged(
		ownerSessionId: string,
		reason: TeamRuntimeChangeReason,
		agentId?: string,
	): void {
		const binding = this.bindings.get(ownerSessionId);
		if (binding) {
			emitTeamRuntimeChanged(binding.events, { ownerSessionId, reason, agentId });
		}
		if (reason !== "spawned" && reason !== "settled" && reason !== "removed") return;
		for (const [sessionId, otherBinding] of this.bindings) {
			if (sessionId === ownerSessionId) continue;
			emitTeamRuntimeChanged(otherBinding.events, {
				ownerSessionId: sessionId,
				reason: "detached_changed",
			});
		}
	}

	private clearConversationSubscriptions(owner: string): void {
		for (const unsubscribe of this.conversationUnsubscribers.get(owner) ?? []) unsubscribe();
	}

	private observe(owner: string, operation: () => void): void {
		try {
			operation();
		} catch {
			try {
				this.reportWarning(owner, "Team conversation capture failed; agent execution continues.");
			} catch {
				/* Observability must not interrupt execution. */
			}
		}
	}

	private notifyConversation(owner: string, id: string): void {
		if (!this.conversationSubscriptions.has(id) || this.conversationNotifications.has(id)) return;
		const timer = setTimeout(() => {
			this.conversationNotifications.delete(id);
			const binding = this.bindings.get(owner);
			if (binding && this.conversationSubscriptions.has(id))
				emitTeamConversationChanged(binding.events, { ownerSessionId: owner, agentId: id });
		}, 0);
		this.conversationNotifications.set(id, timer);
	}

	private removeRecent(id: string): void {
		const entry = this.recentConversations.get(id);
		if (!entry) return;
		this.recentConversations.delete(id);
		const remaining = (this.recentByOwner.get(entry.owner) ?? []).filter(
			(summary) => summary.id !== id,
		);
		if (remaining.length) this.recentByOwner.set(entry.owner, remaining);
		else this.recentByOwner.delete(entry.owner);
		this.notifyConversation(entry.owner, id);
	}

	private cleanRecent(): void {
		for (const [id, entry] of this.recentConversations) {
			if (entry.summary.settledAt + RECENT_AGENT_TTL_MS <= this.now()) this.removeRecent(id);
		}
		this.scheduleExpiry();
	}

	private scheduleExpiry(): void {
		if (this.expiryTimer) clearTimeout(this.expiryTimer);
		this.expiryTimer = undefined;
		let nearest = Infinity;
		for (const entry of this.recentConversations.values())
			nearest = Math.min(nearest, entry.summary.settledAt + RECENT_AGENT_TTL_MS);
		if (nearest === Infinity) return;
		this.expiryTimer = setTimeout(() => this.cleanRecent(), Math.max(1, nearest - this.now()));
		this.expiryTimer.unref?.();
	}

	private rememberSettledAgent(state: SubagentState): void {
		this.cleanRecent();
		const conversation = this.conversations.get(state.id);
		if (!conversation) return;
		const raw = buildActiveAgentSummary(state);
		if (
			[raw.agentName, raw.brief, raw.model, raw.result, raw.error].some(
				(value) => value !== undefined && boundedText(value).truncated,
			)
		)
			conversation.markTruncated();
		const summary: RecentAgentSummary = {
			...raw,
			agentName: boundedText(raw.agentName).text,
			brief: boundedText(raw.brief).text,
			model: raw.model === undefined ? undefined : boundedText(raw.model).text,
			result: raw.result === undefined ? undefined : boundedText(raw.result).text,
			error: raw.error === undefined ? undefined : boundedText(raw.error).text,
			settledAt: this.now(),
		};
		const size = Buffer.byteLength(JSON.stringify(conversation.snapshot(summary)));
		this.recentConversations.set(state.id, {
			owner: state.ownerSessionId,
			conversation,
			summary,
			size,
		});
		this.recentByOwner.set(state.ownerSessionId, [
			summary,
			...(this.recentByOwner.get(state.ownerSessionId) ?? []),
		]);
		const ownerEntries = this.recentByOwner.get(state.ownerSessionId) ?? [];
		for (const entry of ownerEntries.slice(CONVERSATION_RECENT_PER_OWNER))
			this.removeRecent(entry.id);
		let total = Array.from(this.recentConversations.values()).reduce(
			(sum, entry) => sum + entry.size,
			0,
		);
		for (const [id, entry] of this.recentConversations) {
			if (
				this.recentConversations.size <= CONVERSATION_RECENT_MAX_COUNT &&
				total <= CONVERSATION_RECENT_MAX_BYTES
			)
				break;
			total -= entry.size;
			this.removeRecent(id);
		}
		this.scheduleExpiry();
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
		this.observe(state.ownerSessionId, () => {
			const conversation = this.conversations.get(state.id);
			conversation?.endPromptCycle();
			conversation?.touch();
			if (state.status !== "waiting") this.rememberSettledAgent(state);
			this.notifyConversation(state.ownerSessionId, state.id);
		});
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
		this.agents.delete(state.id);
		this.conversations.delete(state.id);
		state.unsubscribe?.();
		state.session?.dispose();
		this.notifyRuntimeChanged(state.ownerSessionId, "removed", state.id);
	}
}

const runtimeKey = Symbol.for("team.runtime");
const globalWithRuntime = globalThis as unknown as { [runtimeKey]: Runtime };

export const team = (globalWithRuntime[runtimeKey] ??= new Runtime());
