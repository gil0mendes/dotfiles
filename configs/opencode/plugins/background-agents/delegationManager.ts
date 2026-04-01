import * as fs from "node:fs/promises";
import * as path from "node:path";

import { generateReadableId } from "../common/generateReadableId";
import { generateMetadata } from "../common/generateMetadata";
import type { Logger } from "../common/logger";
import { parseAgentWriteCapability } from "../common/parseAgentWriteCapability";
import type { OpencodeClient } from "../common/types";

export type DelegationStatus =
	| "registered"
	| "running"
	| "complete"
	| "error"
	| "cancelled"
	| "timeout";

type DelegationTerminalStatus = Extract<
	DelegationStatus,
	"complete" | "error" | "cancelled" | "timeout"
>;

export type DelegationProgress = {
	lastUpdateAt: Date;
	lastHeartbeatAt: Date;
	lastMessage?: string;
	lastMessageAt?: Date;
};

type DelegationNotificationState = {
	terminalNotifiedAt?: Date;
	terminalNotificationCount: number;
};

type ParentNotificationState = {
	allCompleteNotificationCount: number;
	allCompleteCycle: number;
	allCompleteCycleToken: string;
	allCompleteNotifiedCycleToken?: string;
	allCompleteScheduledCycle?: number;
	allCompleteScheduledCycleToken?: string;
	allCompleteScheduledTimer?: ReturnType<typeof setTimeout>;
};

type DelegationRetrievalState = {
	retrievedAt?: Date;
	retrievalCount: number;
	lastReaderSessionID?: string;
};

type DelegationArtifactState = {
	filePath: string;
	persistedAt?: Date;
	byteLength?: number;
	persistError?: string;
};

export type DelegationRecord = {
	id: string;
	rootSessionID: string;
	sessionID: string;
	parentSessionID: string;
	parentMessageID: string;
	parentAgent: string;
	prompt: string;
	agent: string;
	notificationCycle: number;
	notificationCycleToken: string;
	status: DelegationStatus;
	createdAt: Date;
	startedAt?: Date;
	completedAt?: Date;
	updatedAt: Date;
	timeoutAt: Date;
	progress: DelegationProgress;
	notification: DelegationNotificationState;
	retrieval: DelegationRetrievalState;
	artifact: DelegationArtifactState;
	error?: string;
	title?: string;
	description?: string;
	result?: string;
};

type DelegateInput = {
	parentSessionID: string;
	parentMessageID: string;
	parentAgent: string;
	prompt: string;
	agent: string;
};

export type DelegationListItem = {
	id: string;
	status: DelegationStatus;
	title?: string;
	description?: string;
	agent?: string;
	unread?: boolean;
};

type DelegationManagerOptions = {
	maxRunTimeMs?: number;
	readPollIntervalMs?: number;
	terminalWaitGraceMs?: number;
	allCompleteQuietPeriodMs?: number;
	idGenerator?: () => string;
	metadataGenerator?: typeof generateMetadata;
};

type SessionMessageItem = {
	info: {
		role: string;
	};
	parts: Array<{
		type: string;
		text?: string;
	}>;
};

type AssistantSessionMessageItem = {
	info: { role: "assistant" };
	parts: Array<{
		type: string;
		text?: string;
	}>;
};

const DEFAULT_MAX_RUN_TIME_MS = 15 * 60 * 1000;
const TERMINAL_WAIT_GRACE_MS = 10_000;
const READ_POLL_INTERVAL_MS = 250;
const ALL_COMPLETE_QUIET_PERIOD_MS = 50;

function isTerminalStatus(
	status: DelegationStatus,
): status is DelegationTerminalStatus {
	return (
		status === "complete" ||
		status === "error" ||
		status === "cancelled" ||
		status === "timeout"
	);
}

function isActiveStatus(status: DelegationStatus): boolean {
	return status === "registered" || status === "running";
}

function normalizeId(value: string): string {
	return value.trim();
}

function parsePersistedStatus(raw: string): DelegationStatus {
	if (raw === "registered") return "registered";
	if (raw === "running") return "running";
	if (raw === "complete") return "complete";
	if (raw === "error") return "error";
	if (raw === "cancelled") return "cancelled";
	if (raw === "timeout") return "timeout";
	return "error";
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null;
}

function parseSessionMessageInfo(
	value: unknown,
): SessionMessageItem["info"] | undefined {
	if (!isRecord(value)) return undefined;
	if (typeof value.role !== "string") return undefined;
	return { role: value.role };
}

function parseSessionMessagePart(
	value: unknown,
): SessionMessageItem["parts"][number] | undefined {
	if (!isRecord(value)) return undefined;
	if (typeof value.type !== "string") return undefined;
	if (value.text !== undefined && typeof value.text !== "string") return undefined;
	return {
		type: value.type,
		text: typeof value.text === "string" ? value.text : undefined,
	};
}

function parseSessionMessages(data: unknown): SessionMessageItem[] {
	if (!Array.isArray(data)) return [];

	const parsed: SessionMessageItem[] = [];
	for (const item of data) {
		if (!isRecord(item)) continue;
		const info = parseSessionMessageInfo(item.info);
		if (!info) continue;
		const rawParts = item.parts;
		if (!Array.isArray(rawParts)) continue;
		const parts = rawParts.flatMap((part) => {
			const parsedPart = parseSessionMessagePart(part);
			return parsedPart ? [parsedPart] : [];
		});
		parsed.push({ info, parts });
	}

	return parsed;
}

export class DelegationManager {
	private delegations = new Map<string, DelegationRecord>();
	private delegationsBySession = new Map<string, string>();
	private terminalWaiters = new Map<
		string,
		{ promise: Promise<void>; resolve: () => void }
	>();
	private timeoutTimers = new Map<string, ReturnType<typeof setTimeout>>();
	private pendingByParent = new Map<string, Set<string>>();
	private parentNotificationState = new Map<string, ParentNotificationState>();
	private client: OpencodeClient;
	private baseDir: string;
	private log: Logger;
	private maxRunTimeMs: number;
	private readPollIntervalMs: number;
	private terminalWaitGraceMs: number;
	private allCompleteQuietPeriodMs: number;
	private idGenerator: () => string;
	private metadataGenerator: typeof generateMetadata;

	constructor(
		client: OpencodeClient,
		baseDir: string,
		log: Logger,
		options: DelegationManagerOptions = {},
	) {
		this.client = client;
		this.baseDir = baseDir;
		this.log = log;
		this.maxRunTimeMs = options.maxRunTimeMs ?? DEFAULT_MAX_RUN_TIME_MS;
		this.readPollIntervalMs =
			options.readPollIntervalMs ?? READ_POLL_INTERVAL_MS;
		this.terminalWaitGraceMs =
			options.terminalWaitGraceMs ?? TERMINAL_WAIT_GRACE_MS;
		this.allCompleteQuietPeriodMs =
			options.allCompleteQuietPeriodMs ?? ALL_COMPLETE_QUIET_PERIOD_MS;
		this.idGenerator = options.idGenerator ?? generateReadableId;
		this.metadataGenerator = options.metadataGenerator ?? generateMetadata;
	}

	async getRootSessionID(sessionID: string): Promise<string> {
		let currentID = sessionID;
		for (let depth = 0; depth < 10; depth++) {
			try {
				const session = await this.client.session.get({
					path: { id: currentID },
				});

				if (!session.data?.parentID) {
					return currentID;
				}

				currentID = session.data.parentID;
			} catch {
				return currentID;
			}
		}

		return currentID;
	}

	private async getDelegationsDir(sessionID: string): Promise<string> {
		const rootID = await this.getRootSessionID(sessionID);
		return path.join(this.baseDir, rootID);
	}

	private async ensureDelegationsDir(sessionID: string): Promise<string> {
		const dir = await this.getDelegationsDir(sessionID);
		await fs.mkdir(dir, { recursive: true });
		return dir;
	}

	private createTerminalWaiter(id: string): void {
		if (this.terminalWaiters.has(id)) return;

		let resolve: (() => void) | undefined;
		const promise = new Promise<void>((innerResolve) => {
			resolve = innerResolve;
		});

		// Promise executors run synchronously, so resolve is assigned before this
		// branch is evaluated. The undefined type remains for TS narrowing only.
		if (!resolve) {
			throw new Error(
				`Failed to initialize terminal waiter for delegation ${id}`,
			);
		}

		this.terminalWaiters.set(id, { promise, resolve });
	}

	private resolveTerminalWaiter(id: string): void {
		const waiter = this.terminalWaiters.get(id);
		if (!waiter) return;
		waiter.resolve();
	}

	private clearTimeoutTimer(id: string): void {
		const timer = this.timeoutTimers.get(id);
		if (!timer) return;
		clearTimeout(timer);
		this.timeoutTimers.delete(id);
	}

	private scheduleTimeout(id: string): void {
		this.clearTimeoutTimer(id);
		const timer = setTimeout(() => {
			void this.handleTimeout(id);
		}, this.maxRunTimeMs + 5_000);
		this.timeoutTimers.set(id, timer);
	}

	private updateDelegation(
		id: string,
		mutate: (delegation: DelegationRecord, now: Date) => void,
	): DelegationRecord | undefined {
		const delegation = this.delegations.get(id);
		if (!delegation) return undefined;

		const now = new Date();
		mutate(delegation, now);
		delegation.updatedAt = now;
		return delegation;
	}

	private getParentNotificationState(
		parentSessionID: string,
	): ParentNotificationState {
		const existing = this.parentNotificationState.get(parentSessionID);
		if (existing) return existing;

		const initialized: ParentNotificationState = {
			allCompleteNotificationCount: 0,
			allCompleteCycle: 0,
			allCompleteCycleToken: this.buildAllCompleteCycleToken(
				parentSessionID,
				0,
			),
		};
		this.parentNotificationState.set(parentSessionID, initialized);
		return initialized;
	}

	private buildAllCompleteCycleToken(
		parentSessionID: string,
		cycle: number,
	): string {
		return `${parentSessionID}:${cycle}`;
	}

	private cancelScheduledAllComplete(state: ParentNotificationState): void {
		if (state.allCompleteScheduledTimer) {
			clearTimeout(state.allCompleteScheduledTimer);
		}
		state.allCompleteScheduledTimer = undefined;
		state.allCompleteScheduledCycle = undefined;
		state.allCompleteScheduledCycleToken = undefined;
	}

	private resetParentAllCompleteNotificationCycle(
		parentSessionID: string,
	): void {
		const state = this.getParentNotificationState(parentSessionID);
		this.cancelScheduledAllComplete(state);
		state.allCompleteCycle += 1;
		state.allCompleteCycleToken = this.buildAllCompleteCycleToken(
			parentSessionID,
			state.allCompleteCycle,
		);
		state.allCompleteNotifiedCycleToken = undefined;
	}

	private registerDelegation(input: {
		id: string;
		rootSessionID: string;
		sessionID: string;
		parentSessionID: string;
		parentMessageID: string;
		parentAgent: string;
		prompt: string;
		agent: string;
		artifactPath: string;
	}): DelegationRecord {
		if (!this.pendingByParent.has(input.parentSessionID)) {
			this.pendingByParent.set(input.parentSessionID, new Set());
			this.resetParentAllCompleteNotificationCycle(input.parentSessionID);
		}

		const parentNotificationState = this.getParentNotificationState(
			input.parentSessionID,
		);
		const now = new Date();
		const delegation: DelegationRecord = {
			id: input.id,
			rootSessionID: input.rootSessionID,
			sessionID: input.sessionID,
			parentSessionID: input.parentSessionID,
			parentMessageID: input.parentMessageID,
			parentAgent: input.parentAgent,
			prompt: input.prompt,
			agent: input.agent,
			notificationCycle: parentNotificationState.allCompleteCycle,
			notificationCycleToken: parentNotificationState.allCompleteCycleToken,
			status: "registered",
			createdAt: now,
			updatedAt: now,
			timeoutAt: new Date(now.getTime() + this.maxRunTimeMs),
			progress: {
				lastUpdateAt: now,
				lastHeartbeatAt: now,
			},
			notification: {
				terminalNotificationCount: 0,
			},
			retrieval: {
				retrievalCount: 0,
			},
			artifact: {
				filePath: input.artifactPath,
			},
		};

		this.delegations.set(delegation.id, delegation);
		this.delegationsBySession.set(delegation.sessionID, delegation.id);
		this.createTerminalWaiter(delegation.id);
		this.pendingByParent.get(delegation.parentSessionID)?.add(delegation.id);

		return delegation;
	}

	private markStarted(id: string): DelegationRecord | undefined {
		return this.updateDelegation(id, (delegation, now) => {
			if (isTerminalStatus(delegation.status)) return;
			delegation.status = "running";
			delegation.startedAt = now;
			delegation.progress.lastUpdateAt = now;
			delegation.progress.lastHeartbeatAt = now;
		});
	}

	private markProgress(
		id: string,
		messageText?: string,
	): DelegationRecord | undefined {
		return this.updateDelegation(id, (delegation, now) => {
			if (isTerminalStatus(delegation.status)) return;
			if (delegation.status === "registered") {
				delegation.status = "running";
				delegation.startedAt = delegation.startedAt ?? now;
			}

			delegation.progress.lastUpdateAt = now;
			delegation.progress.lastHeartbeatAt = now;

			if (messageText) {
				delegation.progress.lastMessage = messageText;
				delegation.progress.lastMessageAt = now;
			}
		});
	}

	private markTerminal(
		id: string,
		status: DelegationTerminalStatus,
		error?: string,
	): { transitioned: boolean; delegation?: DelegationRecord } {
		const delegation = this.delegations.get(id);
		if (!delegation) return { transitioned: false };

		if (isTerminalStatus(delegation.status)) {
			return { transitioned: false, delegation };
		}

		const now = new Date();
		delegation.status = status;
		delegation.completedAt = now;
		delegation.updatedAt = now;
		if (error) delegation.error = error;

		const pending = this.pendingByParent.get(delegation.parentSessionID);
		if (pending) {
			pending.delete(delegation.id);
			if (pending.size === 0) {
				this.pendingByParent.delete(delegation.parentSessionID);
			}
		}

		this.clearTimeoutTimer(id);
		this.resolveTerminalWaiter(id);

		return { transitioned: true, delegation };
	}

	private markNotified(id: string): DelegationRecord | undefined {
		return this.updateDelegation(id, (delegation, now) => {
			delegation.notification.terminalNotifiedAt = now;
			delegation.notification.terminalNotificationCount += 1;
		});
	}

	private markRetrieved(
		id: string,
		readerSessionID: string,
	): DelegationRecord | undefined {
		return this.updateDelegation(id, (delegation, now) => {
			delegation.retrieval.retrievedAt = now;
			delegation.retrieval.retrievalCount += 1;
			delegation.retrieval.lastReaderSessionID = readerSessionID;
		});
	}

	private hasUnreadCompletion(delegation: DelegationRecord): boolean {
		if (!isTerminalStatus(delegation.status)) return false;
		// Only surface terminal results after notifyParent established the parent
		// notification anchor. In current paths, this is set before the async send.
		if (!delegation.notification.terminalNotifiedAt) return false;
		if (!delegation.completedAt) return false;
		if (!delegation.retrieval.retrievedAt) return true;
		return (
			delegation.retrieval.retrievedAt.getTime() <
			delegation.completedAt.getTime()
		);
	}

	private async waitForTerminal(
		id: string,
		timeoutMs: number,
	): Promise<"terminal" | "timeout"> {
		const delegation = this.delegations.get(id);
		if (!delegation) return "timeout";
		if (isTerminalStatus(delegation.status)) return "terminal";

		const waiter = this.terminalWaiters.get(id);
		if (!waiter) return "timeout";

		let timer: ReturnType<typeof setTimeout> | undefined;
		try {
			return await Promise.race<"terminal" | "timeout">([
				waiter.promise.then(() => "terminal"),
				new Promise<"timeout">((resolve) => {
					timer = setTimeout(() => resolve("timeout"), timeoutMs);
				}),
			]);
		} finally {
			if (timer) clearTimeout(timer);
		}
	}

	private async generateUniqueDelegationId(
		artifactDir: string,
	): Promise<string> {
		for (let attempt = 0; attempt < 20; attempt++) {
			const candidate = this.idGenerator();
			if (this.delegations.has(candidate)) continue;

			const candidatePath = path.join(artifactDir, `${candidate}.md`);
			try {
				await fs.access(candidatePath);
			} catch {
				return candidate;
			}
		}

		throw new Error(
			"Failed to generate unique delegation ID after 20 attempts",
		);
	}

	private isVisibleToSession(
		delegation: DelegationRecord,
		rootSessionID: string,
	): boolean {
		return delegation.rootSessionID === rootSessionID;
	}

	private buildTerminalNotification(
		delegation: DelegationRecord,
		remainingCount: number,
	): string {
		const lines = [
			"<task-notification>",
			`<task-id>${delegation.id}</task-id>`,
			`<status>${delegation.status}</status>`,
			`<summary>Background agent ${delegation.status}: ${delegation.title || delegation.id}</summary>`,
			delegation.title ? `<title>${delegation.title}</title>` : "",
			delegation.description
				? `<description>${delegation.description}</description>`
				: "",
			delegation.error ? `<error>${delegation.error}</error>` : "",
			`<artifact>${delegation.artifact.filePath}</artifact>`,
			`<retrieval>Use delegation_read("${delegation.id}") for full output.</retrieval>`,
			remainingCount > 0 ? `<remaining>${remainingCount}</remaining>` : "",
			"</task-notification>",
		];

		return lines.filter((line) => line.length > 0).join("\n");
	}

	private buildAllCompleteNotification(
		parentSessionID: string,
		cycle: number,
		cycleToken: string,
	): string {
		return [
			"<task-notification>",
			"<type>all-complete</type>",
			"<status>completed</status>",
			"<summary>All delegations complete.</summary>",
			`<parent-session-id>${parentSessionID}</parent-session-id>`,
			`<cycle>${cycle}</cycle>`,
			`<cycle-token>${cycleToken}</cycle-token>`,
			"</task-notification>",
		].join("\n");
	}

	private buildDeterministicTerminalReadResponse(
		delegation: DelegationRecord,
	): string {
		const lines = [
			`Delegation ID: ${delegation.id}`,
			`Status: ${delegation.status}`,
			`Agent: ${delegation.agent}`,
			`Started: ${(delegation.startedAt ?? delegation.createdAt).toISOString()}`,
			`Completed: ${delegation.completedAt?.toISOString() || "N/A"}`,
			`Artifact: ${delegation.artifact.filePath}`,
		];

		if (delegation.title) lines.push(`Title: ${delegation.title}`);
		if (delegation.description)
			lines.push(`Description: ${delegation.description}`);
		if (delegation.error) lines.push(`Error: ${delegation.error}`);

		lines.push(
			`\nUse delegation_read("${delegation.id}") again after persistence completes.`,
		);
		return lines.join("\n");
	}

	private async readPersistedArtifact(
		filePath: string,
	): Promise<string | null> {
		try {
			return await fs.readFile(filePath, "utf8");
		} catch {
			return null;
		}
	}

	private async waitForPersistedArtifact(
		filePath: string,
		maxWaitMs: number,
	): Promise<string | null> {
		const start = Date.now();
		// TODO: switch to file-watch if delegation completion volume grows.
		let waitMs = this.readPollIntervalMs;
		while (Date.now() - start < maxWaitMs) {
			const content = await this.readPersistedArtifact(filePath);
			if (content !== null) return content;
			await new Promise((resolve) => setTimeout(resolve, waitMs));
			waitMs = Math.min(waitMs * 2, 1_000);
		}

		return null;
	}

	private async resolveDelegationResult(
		delegation: DelegationRecord,
	): Promise<string> {
		if (delegation.status === "error") {
			return `Error: ${delegation.error || "Delegation failed."}`;
		}

		if (delegation.status === "cancelled") {
			return "Delegation was cancelled before completion.";
		}

		if (delegation.status === "timeout") {
			const partial = await this.getResult(delegation);
			return `${partial}\n\n[TIMEOUT REACHED]`;
		}

		return await this.getResult(delegation);
	}

	private areCycleTerminalNotificationsComplete(
		parentSessionID: string,
		cycleToken: string,
	): boolean {
		let cycleDelegationCount = 0;

		for (const delegation of this.delegations.values()) {
			if (delegation.parentSessionID !== parentSessionID) continue;
			if (delegation.notificationCycleToken !== cycleToken) continue;

			cycleDelegationCount += 1;
			if (!delegation.notification.terminalNotifiedAt) {
				return false;
			}
		}

		return cycleDelegationCount > 0;
	}

	private scheduleAllCompleteForParent(
		parentSessionID: string,
		parentAgent: string,
	): void {
		const state = this.getParentNotificationState(parentSessionID);
		const cycle = state.allCompleteCycle;
		const cycleToken = state.allCompleteCycleToken;

		if (
			!this.areCycleTerminalNotificationsComplete(parentSessionID, cycleToken)
		) {
			return;
		}
		if (state.allCompleteNotifiedCycleToken === cycleToken) return;
		if (state.allCompleteScheduledCycleToken === cycleToken) return;

		this.cancelScheduledAllComplete(state);
		state.allCompleteScheduledCycle = cycle;
		state.allCompleteScheduledCycleToken = cycleToken;
		state.allCompleteScheduledTimer = setTimeout(() => {
			void this.dispatchScheduledAllComplete(
				parentSessionID,
				parentAgent,
				cycle,
				cycleToken,
			);
		}, this.allCompleteQuietPeriodMs);
	}

	private async dispatchScheduledAllComplete(
		parentSessionID: string,
		parentAgent: string,
		cycle: number,
		cycleToken: string,
	): Promise<void> {
		const state = this.getParentNotificationState(parentSessionID);

		if (state.allCompleteScheduledCycleToken !== cycleToken) return;
		this.cancelScheduledAllComplete(state);
		if (state.allCompleteCycleToken !== cycleToken) return;
		if (
			!this.areCycleTerminalNotificationsComplete(parentSessionID, cycleToken)
		) {
			return;
		}
		if (state.allCompleteNotifiedCycleToken === cycleToken) return;

		try {
			await this.client.session.prompt({
				path: { id: parentSessionID },
				body: {
					noReply: false,
					agent: parentAgent,
					parts: [
						{
							type: "text",
							text: this.buildAllCompleteNotification(
								parentSessionID,
								cycle,
								cycleToken,
							),
						},
					],
				},
			});
		} catch (error) {
			await this.debugLog(
				`all-complete notification failed for ${parentSessionID} cycle=${cycleToken}: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
			return;
		}

		if (state.allCompleteCycleToken !== cycleToken) return;
		if (
			!this.areCycleTerminalNotificationsComplete(parentSessionID, cycleToken)
		) {
			return;
		}

		state.allCompleteNotificationCount += 1;
		state.allCompleteNotifiedCycleToken = cycleToken;
	}

	private async notifyParent(delegationID: string): Promise<void> {
		try {
			const delegation = this.delegations.get(delegationID);
			if (!delegation) return;
			if (!isTerminalStatus(delegation.status)) return;
			if (delegation.notification.terminalNotifiedAt) {
				await this.debugLog(
					`notifyParent skipped for ${delegation.id}; already notified`,
				);
				return;
			}

			const remainingCount = this.getPendingCount(delegation.parentSessionID);
			const terminalNotification = this.buildTerminalNotification(
				delegation,
				remainingCount,
			);
			this.markNotified(delegation.id);

			await this.client.session.prompt({
				path: { id: delegation.parentSessionID },
				body: {
					noReply: true,
					agent: delegation.parentAgent,
					parts: [{ type: "text", text: terminalNotification }],
				},
			});
			this.scheduleAllCompleteForParent(
				delegation.parentSessionID,
				delegation.parentAgent,
			);

			await this.debugLog(
				`notifyParent sent for ${delegation.id} (remaining=${remainingCount}, status=${delegation.status})`,
			);
		} catch (error) {
			await this.debugLog(
				`notifyParent failed for ${delegationID}: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
		}
	}

	private async finalizeDelegation(
		delegationID: string,
		status: DelegationTerminalStatus,
		error?: string,
	): Promise<void> {
		const { transitioned, delegation } = this.markTerminal(
			delegationID,
			status,
			error,
		);
		if (!transitioned || !delegation) return;

		await this.debugLog(
			`finalizeDelegation(${delegation.id}, ${status}) started`,
		);

		const resolvedResult = await this.resolveDelegationResult(delegation);
		delegation.result = resolvedResult;

		if (resolvedResult.trim().length > 0) {
			const metadata = await this.metadataGenerator(
				this.client,
				resolvedResult,
				delegation.sessionID,
				(msg) => this.debugLog(msg),
			);
			delegation.title = metadata.title;
			delegation.description = metadata.description;
		}

		await this.persistOutput(delegation, resolvedResult);
		await this.notifyParent(delegation.id);
	}

	async delegate(input: DelegateInput): Promise<DelegationRecord> {
		const agentsResult = await this.client.app.agents({});
		const rawAgents = agentsResult.data;
		const agents = Array.isArray(rawAgents) ? rawAgents : [];
		const validAgent = agents.find(
			(agent) => isRecord(agent) && agent.name === input.agent,
		);

		if (!validAgent) {
			const available = agents
				.filter(
					(agent) =>
						isRecord(agent) &&
						(typeof agent.mode !== "string" ||
							agent.mode === "subagent" ||
							agent.mode === "all"),
				)
				.map((agent) => {
					if (!isRecord(agent) || typeof agent.name !== "string")
						return "• (unknown)";
					const description =
						typeof agent.description === "string"
							? ` - ${agent.description}`
							: "";
					return `• ${agent.name}${description}`;
				})
				.join("\n");

			throw new Error(
				`Agent "${input.agent}" not found.\n\nAvailable agents:\n${available || "(none)"}`,
			);
		}

		const { isReadOnly } = await parseAgentWriteCapability(
			this.client,
			input.agent,
			this.log,
		);
		if (!isReadOnly) {
			throw new Error(
				`Agent "${input.agent}" is write-capable and requires the native \`task\` tool for proper undo/branching support.\n\n` +
					`Use \`task\` instead of \`delegate\` for write-capable agents.\n\n` +
					`Read-only sub-agents (edit/write/bash denied) use \`delegate\`.\n` +
					`Write-capable sub-agents (any write permission) use \`task\`.`,
			);
		}

		const artifactDir = await this.ensureDelegationsDir(input.parentSessionID);
		const rootSessionID = await this.getRootSessionID(input.parentSessionID);
		const stableID = await this.generateUniqueDelegationId(artifactDir);
		const artifactPath = path.join(artifactDir, `${stableID}.md`);

		await this.debugLog(`delegate() called, generated stable ID: ${stableID}`);

		const sessionResult = await this.client.session.create({
			body: {
				title: `Delegation: ${stableID}`,
				parentID: input.parentSessionID,
			},
		});

		await this.debugLog(
			`session.create result: ${JSON.stringify(sessionResult.data)}`,
		);

		if (!sessionResult.data?.id) {
			throw new Error("Failed to create delegation session");
		}

		const delegation = this.registerDelegation({
			id: stableID,
			rootSessionID,
			sessionID: sessionResult.data.id,
			parentSessionID: input.parentSessionID,
			parentMessageID: input.parentMessageID,
			parentAgent: input.parentAgent,
			prompt: input.prompt,
			agent: input.agent,
			artifactPath,
		});

		await this.debugLog(
			`Registered delegation ${delegation.id} before execution`,
		);
		this.scheduleTimeout(delegation.id);
		this.markStarted(delegation.id);

		this.client.session
			.prompt({
				path: { id: delegation.sessionID },
				body: {
					agent: input.agent,
					parts: [{ type: "text", text: input.prompt }],
					tools: {
						task: false,
						delegate: false,
						todowrite: false,
						plan_save: false,
					},
				},
			})
			.catch((error: Error) => {
				void this.finalizeDelegation(delegation.id, "error", error.message);
			});

		return delegation;
	}

	private async handleTimeout(delegationID: string): Promise<void> {
		const delegation = this.delegations.get(delegationID);
		if (!delegation || isTerminalStatus(delegation.status)) return;

		await this.debugLog(`handleTimeout for delegation ${delegation.id}`);

		try {
			await this.client.session.delete({
				path: { id: delegation.sessionID },
			});
		} catch {
			// Ignore.
		}

		await this.finalizeDelegation(
			delegation.id,
			"timeout",
			`Delegation timed out after ${this.maxRunTimeMs / 1000}s`,
		);
	}

	async handleSessionIdle(sessionID: string): Promise<void> {
		const delegation = this.findBySession(sessionID);
		if (!delegation || isTerminalStatus(delegation.status)) return;

		await this.debugLog(`handleSessionIdle for delegation ${delegation.id}`);
		await this.finalizeDelegation(delegation.id, "complete");
	}

	private async getResult(delegation: DelegationRecord): Promise<string> {
		try {
			const messages = await this.client.session.messages({
				path: { id: delegation.sessionID },
			});
			const messageData = parseSessionMessages(messages.data);

			if (messageData.length === 0) {
				await this.debugLog(
					`getResult: No messages found for session ${delegation.sessionID}`,
				);
				return `Delegation "${delegation.description || delegation.id}" completed but produced no output.`;
			}

			await this.debugLog(
				`getResult: Found ${messageData.length} messages. Roles: ${messageData.map((message) => message.info.role).join(", ")}`,
			);

			const assistantMessages = messageData.filter(
				(message): message is AssistantSessionMessageItem =>
					message.info.role === "assistant",
			);

			if (assistantMessages.length === 0) {
				return `Delegation "${delegation.description || delegation.id}" completed but produced no assistant response.`;
			}

			const lastMessage = assistantMessages[assistantMessages.length - 1];
			if (!lastMessage) {
				return `Delegation "${delegation.description || delegation.id}" completed but produced no assistant response.`;
			}
			const textParts = lastMessage.parts.filter(
				(
					part,
				): part is AssistantSessionMessageItem["parts"][number] & {
					type: "text";
					text: string;
				} => part.type === "text" && typeof part.text === "string",
			);

			if (textParts.length === 0) {
				return `Delegation "${delegation.description || delegation.id}" completed but produced no text content.`;
			}

			return textParts.map((part) => part.text).join("\n");
		} catch (error) {
			await this.debugLog(
				`getResult error: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
			return `Delegation "${delegation.description || delegation.id}" completed but result could not be retrieved: ${error instanceof Error ? error.message : "Unknown error"}`;
		}
	}

	private async persistOutput(
		delegation: DelegationRecord,
		content: string,
	): Promise<void> {
		try {
			const title = delegation.title || delegation.id;
			const description =
				delegation.description || "(No description generated)";
			const header = `# ${title}

${description}

**ID:** ${delegation.id}
**Agent:** ${delegation.agent}
**Status:** ${delegation.status}
**Session:** ${delegation.sessionID}
**Started:** ${(delegation.startedAt || delegation.createdAt).toISOString()}
**Completed:** ${delegation.completedAt?.toISOString() || "N/A"}

---

`;

			await fs.writeFile(
				delegation.artifact.filePath,
				header + content,
				"utf8",
			);

			const stats = await fs.stat(delegation.artifact.filePath);
			this.updateDelegation(delegation.id, (record, now) => {
				record.artifact.persistedAt = now;
				record.artifact.byteLength = stats.size;
				record.artifact.persistError = undefined;
			});

			await this.debugLog(
				`Persisted output to ${delegation.artifact.filePath}`,
			);
		} catch (error) {
			this.updateDelegation(delegation.id, (record) => {
				record.artifact.persistError =
					error instanceof Error ? error.message : "Unknown persistence error";
			});
			await this.debugLog(
				`Failed to persist output: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
		}
	}

	async readOutput(sessionID: string, id: string): Promise<string> {
		const normalizedID = normalizeId(id);
		if (!normalizedID) {
			throw new Error("Delegation ID is required");
		}

		const rootSessionID = await this.getRootSessionID(sessionID);
		let delegation = this.delegations.get(normalizedID);
		if (delegation && !this.isVisibleToSession(delegation, rootSessionID)) {
			delegation = undefined;
		}

		const fallbackFilePath = path.join(
			await this.getDelegationsDir(sessionID),
			`${normalizedID}.md`,
		);
		const immediateArtifactPath =
			delegation?.artifact.filePath || fallbackFilePath;
		const immediateRead = await this.readPersistedArtifact(
			immediateArtifactPath,
		);
		if (immediateRead !== null) {
			if (delegation) this.markRetrieved(delegation.id, sessionID);
			return immediateRead;
		}

		if (!delegation) {
			throw new Error(
				`Delegation "${normalizedID}" not found.\n\nUse delegation_list() to see available delegations.`,
			);
		}

		if (isActiveStatus(delegation.status)) {
			const remainingMs = Math.max(
				delegation.timeoutAt.getTime() - Date.now() + this.terminalWaitGraceMs,
				this.readPollIntervalMs,
			);

			await this.debugLog(
				`readOutput: waiting up to ${remainingMs}ms for delegation ${delegation.id} to reach terminal state`,
			);

			const waitResult = await this.waitForTerminal(delegation.id, remainingMs);
			if (waitResult === "timeout" && isActiveStatus(delegation.status)) {
				// markTerminal() is idempotent, so this remains safe if the timer path
				// already started finalization for the same delegation.
				await this.handleTimeout(delegation.id);
			}
		}

		if (isTerminalStatus(delegation.status)) {
			const delayedPersisted = await this.waitForPersistedArtifact(
				delegation.artifact.filePath,
				Math.max(this.readPollIntervalMs * 8, 500),
			);
			if (delayedPersisted !== null) {
				this.markRetrieved(delegation.id, sessionID);
				return delayedPersisted;
			}
		}

		const persisted = await this.readPersistedArtifact(
			delegation.artifact.filePath,
		);
		if (persisted !== null) {
			this.markRetrieved(delegation.id, sessionID);
			return persisted;
		}

		if (isTerminalStatus(delegation.status)) {
			return this.buildDeterministicTerminalReadResponse(delegation);
		}

		return `Delegation "${delegation.id}" is still running. You will receive a <task-notification> when it reaches a terminal state.`;
	}

	async listDelegations(sessionID: string): Promise<DelegationListItem[]> {
		const rootSessionID = await this.getRootSessionID(sessionID);
		const results: DelegationListItem[] = [];

		for (const delegation of this.delegations.values()) {
			if (!this.isVisibleToSession(delegation, rootSessionID)) continue;

			results.push({
				id: delegation.id,
				status: delegation.status,
				title: delegation.title || delegation.id,
				description:
					delegation.description ||
					(isActiveStatus(delegation.status)
						? "(running)"
						: "(no description)"),
				agent: delegation.agent,
				unread: this.hasUnreadCompletion(delegation),
			});
		}

		try {
			const dir = await this.getDelegationsDir(rootSessionID);
			const files = await fs.readdir(dir);

			for (const file of files) {
				if (!file.endsWith(".md")) continue;

				const id = file.replace(".md", "");
				if (results.find((result) => result.id === id)) continue;

				let title = "(loaded from storage)";
				let description = "";
				let agent: string | undefined;
				let status: DelegationStatus = "complete";

				try {
					const filePath = path.join(dir, file);
					const content = await fs.readFile(filePath, "utf8");
					const titleMatch = content.match(/^# (.+)$/m);
					if (titleMatch?.[1]) title = titleMatch[1];
					const agentMatch = content.match(/^\*\*Agent:\*\* (.+)$/m);
					if (agentMatch?.[1]) agent = agentMatch[1];
					const statusMatch = content.match(/^\*\*Status:\*\* (.+)$/m);
					const rawStatus = statusMatch?.[1]?.trim();
					if (rawStatus) {
						status = parsePersistedStatus(rawStatus);
					}

					const lines = content.split("\n");
					if (lines.length > 2 && lines[2]) {
						description = lines[2].slice(0, 150);
					}
				} catch {
					// Ignore read errors.
				}

				results.push({
					id,
					status,
					title,
					description,
					agent,
					unread: false,
				});
			}
		} catch {
			// Directory may not exist yet.
		}

		results.sort((left, right) => left.id.localeCompare(right.id));
		return results;
	}

	async deleteDelegation(sessionID: string, id: string): Promise<boolean> {
		const normalizedID = normalizeId(id);
		const delegation = this.delegations.get(normalizedID);
		const deletedWhileActive = delegation ? isActiveStatus(delegation.status) : false;

		if (delegation) {
			this.clearTimeoutTimer(delegation.id);

			if (deletedWhileActive) {
				try {
					await this.client.session.delete({
						path: { id: delegation.sessionID },
					});
				} catch {
					// Session may already be deleted.
				}
				await this.finalizeDelegation(
					delegation.id,
					"cancelled",
					"Delegation deleted by cleanup",
				);
			}

			this.terminalWaiters.delete(delegation.id);
			this.delegationsBySession.delete(delegation.sessionID);
			this.delegations.delete(delegation.id);
		}

		if (deletedWhileActive) {
			return true;
		}

		try {
			const dir = await this.getDelegationsDir(sessionID);
			const filePath = path.join(dir, `${normalizedID}.md`);
			await fs.unlink(filePath);
			return true;
		} catch {
			return false;
		}
	}

	findBySession(sessionID: string): DelegationRecord | undefined {
		const delegationID = this.delegationsBySession.get(sessionID);
		if (!delegationID) return undefined;
		return this.delegations.get(delegationID);
	}

	handleMessageEvent(sessionID: string, messageText?: string): void {
		const delegation = this.findBySession(sessionID);
		if (!delegation) return;
		this.markProgress(delegation.id, messageText);
	}

	getPendingCount(parentSessionID: string): number {
		const pendingSet = this.pendingByParent.get(parentSessionID);
		if (!pendingSet) return 0;

		return Array.from(pendingSet).filter((id) => {
			const delegation = this.delegations.get(id);
			return delegation ? isActiveStatus(delegation.status) : false;
		}).length;
	}

	getRunningDelegations(rootSessionID?: string): DelegationRecord[] {
		return Array.from(this.delegations.values()).filter((delegation) => {
			if (rootSessionID && delegation.rootSessionID !== rootSessionID)
				return false;
			return isActiveStatus(delegation.status);
		});
	}

	getUnreadCompletedDelegations(
		rootSessionID: string,
		limit: number = 10,
	): DelegationRecord[] {
		return Array.from(this.delegations.values())
			.filter((delegation) => delegation.rootSessionID === rootSessionID)
			.filter((delegation) => this.hasUnreadCompletion(delegation))
			.sort((left, right) => {
				const leftTime = left.completedAt?.getTime() || 0;
				const rightTime = right.completedAt?.getTime() || 0;
				return rightTime - leftTime;
			})
			.slice(0, limit);
	}

	async getRecentCompletedDelegations(
		sessionID: string,
		limit: number = 10,
	): Promise<DelegationListItem[]> {
		const all = await this.listDelegations(sessionID);
		return all
			.filter((delegation) => isTerminalStatus(delegation.status))
			.slice(-limit);
	}

	async debugLog(msg: string): Promise<void> {
		const timestamp = new Date().toISOString();
		const line = `${timestamp}: ${msg}\n`;
		const debugFile = path.join(this.baseDir, "background-agents-debug.log");

		try {
			await fs.appendFile(debugFile, line, "utf8");
		} catch {
			// Ignore debug logging errors.
		}
	}
}
