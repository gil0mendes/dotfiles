import type { MetricsRecorder, ModelUsageSample, SessionStartType } from "./metrics";

export interface ToolExecutionOutcome {
	toolName: string;
	isError: boolean;
	durationMs?: number;
	/** Set by Pi when another tool made this call. */
	parentToolCallId?: string;
}

/** Translates live Pi events into recorder calls. Holds per-session timing state only. */
export interface SessionObserver {
	sessionStarted(startType: SessionStartType): void;
	/** Records the session duration and settles a run that was still in flight as aborted. */
	sessionEnded(): void;
	agentStarted(): void;
	agentSettled(aborted: boolean): void;
	messageEnded(message: unknown): void;
	toolExecutionEnded(outcome: ToolExecutionOutcome): void;
}

export interface SessionObserverOptions {
	recorder: MetricsRecorder;
	/** Monotonic clock in milliseconds. */
	now?: () => number;
}

const FINALIZED_STOP_REASONS: ReadonlySet<unknown> = new Set([
	"stop",
	"length",
	"toolUse",
	"error",
	"aborted",
]);

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null;
}

function nonNegativeNumber(value: unknown): number {
	return typeof value === "number" && Number.isFinite(value) && value >= 0 ? value : 0;
}

function nonEmptyString(value: unknown): string | undefined {
	return typeof value === "string" && value !== "" ? value : undefined;
}

/** Returns usage only for finalized assistant messages; streaming and deferred ones are skipped. */
function parseFinalizedAssistantUsage(message: unknown): ModelUsageSample | undefined {
	if (!isRecord(message) || message.role !== "assistant") return undefined;
	if (!FINALIZED_STOP_REASONS.has(message.stopReason)) return undefined;

	const model = nonEmptyString(message.model);
	const provider = nonEmptyString(message.provider);
	if (model === undefined || provider === undefined) return undefined;
	if (!isRecord(message.usage)) return undefined;

	const { usage } = message;
	const cost = isRecord(usage.cost) ? usage.cost : {};
	return {
		model,
		provider,
		tokens: {
			input: nonNegativeNumber(usage.input),
			output: nonNegativeNumber(usage.output),
			cacheRead: nonNegativeNumber(usage.cacheRead),
			cacheWrite: nonNegativeNumber(usage.cacheWrite),
		},
		costUsd: nonNegativeNumber(cost.total),
		costSource: cost.totalFromProvider === true ? "provider" : "estimate",
	};
}

function toSeconds(milliseconds: number | undefined): number | undefined {
	if (milliseconds === undefined || !Number.isFinite(milliseconds) || milliseconds < 0) {
		return undefined;
	}
	return milliseconds / 1000;
}

export function createSessionObserver({
	recorder,
	now = () => performance.now(),
}: SessionObserverOptions): SessionObserver {
	const recordedMessages = new WeakSet<object>();
	let sessionStartedAt: number | undefined;
	let interactionStartedAt: number | undefined;

	function settleInteraction(aborted: boolean): void {
		if (interactionStartedAt === undefined) return;
		const durationSeconds = (now() - interactionStartedAt) / 1000;
		interactionStartedAt = undefined;
		recorder.recordInteraction({ durationSeconds, aborted });
	}

	return {
		sessionStarted(startType) {
			sessionStartedAt = now();
			recorder.recordSessionStarted(startType);
		},
		sessionEnded() {
			// Quitting mid-run emits no agent_settled; keep that run's time instead of dropping it.
			settleInteraction(true);
			if (sessionStartedAt === undefined) return;
			const durationSeconds = (now() - sessionStartedAt) / 1000;
			sessionStartedAt = undefined;
			recorder.recordSessionEnded(durationSeconds);
		},
		agentStarted() {
			// One interaction spans automatic retries and continuations until the run settles.
			interactionStartedAt ??= now();
		},
		agentSettled(aborted) {
			settleInteraction(aborted);
		},
		messageEnded(message) {
			if (!isRecord(message) || recordedMessages.has(message)) return;
			const usage = parseFinalizedAssistantUsage(message);
			if (usage === undefined) return;
			recordedMessages.add(message);
			recorder.recordModelUsage(usage);
		},
		toolExecutionEnded({ toolName, isError, durationMs, parentToolCallId }) {
			recorder.recordToolExecution({
				toolName,
				succeeded: !isError,
				nested: parentToolCallId !== undefined,
				durationSeconds: toSeconds(durationMs),
			});
		},
	};
}
