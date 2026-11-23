import type { MetricsConfig } from "./config";

export type SessionStartType = "fresh" | "resume";

export interface TokenCounts {
	input: number;
	output: number;
	cacheRead: number;
	cacheWrite: number;
}

export interface ModelUsageSample {
	model: string;
	provider: string;
	/** Reasoning tokens are a subset of `output` and are deliberately not tracked separately. */
	tokens: TokenCounts;
	costUsd: number;
	costSource: "provider" | "estimate";
}

export interface InteractionSample {
	durationSeconds: number;
	aborted: boolean;
}

export interface ToolExecutionSample {
	toolName: string;
	succeeded: boolean;
	/** Called by another tool (for example a codemode script) rather than by the model. */
	nested: boolean;
	durationSeconds: number | undefined;
}

/** Domain-level recording surface; each schema decides which metrics it can honestly emit. */
export interface MetricsRecorder {
	recordSessionStarted(startType: SessionStartType): void;
	recordSessionEnded(durationSeconds: number): void;
	recordModelUsage(sample: ModelUsageSample): void;
	recordInteraction(sample: InteractionSample): void;
	recordToolExecution(sample: ToolExecutionSample): void;
}

export type MetricAttributes = Record<string, string>;

interface InstrumentOptions {
	description: string;
	unit?: string;
}

export interface CounterLike {
	add(value: number, attributes?: MetricAttributes): void;
}

export interface HistogramLike {
	record(value: number, attributes?: MetricAttributes): void;
}

/** The subset of the OpenTelemetry `Meter` this extension needs; keeps tests free of the SDK. */
export interface MetricsMeter {
	createCounter(name: string, options: InstrumentOptions): CounterLike;
	createHistogram(name: string, options: InstrumentOptions): HistogramLike;
}

function addWhenPositive(counter: CounterLike, value: number, attributes: MetricAttributes): void {
	if (value > 0) counter.add(value, attributes);
}

function outcomeOf(succeeded: boolean): "success" | "error" {
	return succeeded ? "success" : "error";
}

function tokenEntries(
	tokens: TokenCounts,
	cacheWriteType: string,
): ReadonlyArray<readonly [type: string, count: number]> {
	return [
		["input", tokens.input],
		["output", tokens.output],
		["cacheRead", tokens.cacheRead],
		[cacheWriteType, tokens.cacheWrite],
	];
}

/**
 * Pi-native metrics: usage, cost, sessions, interactions (agent runs) and tool executions.
 * Usage metrics carry `model` and `provider`; no identity or session identifiers are attached.
 */
export function createPiMetricsRecorder(meter: MetricsMeter, prefix: string): MetricsRecorder {
	const sessions = meter.createCounter(`${prefix}.session.count`, {
		description: "Count of sessions started",
	});
	const sessionDuration = meter.createHistogram(`${prefix}.session.duration`, {
		description: "Duration of finished sessions",
		unit: "s",
	});
	const tokens = meter.createCounter(`${prefix}.token.usage`, {
		description: "Number of tokens used, counted once per finalized assistant message",
		unit: "tokens",
	});
	const cost = meter.createCounter(`${prefix}.cost.usage`, {
		description: "Cost of model usage, counted once per finalized assistant message",
		unit: "USD",
	});
	const interactions = meter.createCounter(`${prefix}.interaction.count`, {
		description: "Count of settled agent runs",
	});
	const interactionDuration = meter.createHistogram(`${prefix}.interaction.duration`, {
		description: "Wall-clock duration of settled agent runs",
		unit: "s",
	});
	const toolCalls = meter.createCounter(`${prefix}.tool.count`, {
		description: "Count of tool executions",
	});
	const toolDuration = meter.createHistogram(`${prefix}.tool.duration`, {
		description: "Duration of tool executions",
		unit: "s",
	});

	return {
		recordSessionStarted(startType) {
			sessions.add(1, { start_type: startType });
		},
		recordSessionEnded(durationSeconds) {
			sessionDuration.record(durationSeconds);
		},
		recordModelUsage({ model, provider, tokens: counts, costUsd, costSource }) {
			for (const [type, count] of tokenEntries(counts, "cacheWrite")) {
				addWhenPositive(tokens, count, { type, model, provider });
			}
			addWhenPositive(cost, costUsd, { model, provider, cost_source: costSource });
		},
		recordInteraction({ durationSeconds, aborted }) {
			const attributes = { outcome: aborted ? "aborted" : "completed" };
			interactions.add(1, attributes);
			interactionDuration.record(durationSeconds, attributes);
		},
		recordToolExecution({ toolName, succeeded, nested, durationSeconds }) {
			// A nested call runs inside its caller's duration, so consumers need to tell them apart.
			const attributes = {
				tool_name: toolName,
				outcome: outcomeOf(succeeded),
				nested: nested ? "true" : "false",
			};
			toolCalls.add(1, attributes);
			if (durationSeconds !== undefined) toolDuration.record(durationSeconds, attributes);
		},
	};
}

/**
 * Subset of the documented Claude Code metric contract that Pi can produce accurately:
 * `session.count`, `token.usage`, `cost.usage` and `active_time.total{type=cli}`.
 * Attribute names and values mirror https://code.claude.com/docs/en/monitoring-usage;
 * everything Pi cannot measure is left out rather than approximated.
 */
export function createClaudeCodeMetricsRecorder(
	meter: MetricsMeter,
	prefix: string,
): MetricsRecorder {
	const sessions = meter.createCounter(`${prefix}.session.count`, {
		description: "Count of sessions started",
	});
	const cost = meter.createCounter(`${prefix}.cost.usage`, {
		description: "Cost of the session",
		unit: "USD",
	});
	const tokens = meter.createCounter(`${prefix}.token.usage`, {
		description: "Number of tokens used",
		unit: "tokens",
	});
	const activeTime = meter.createCounter(`${prefix}.active_time.total`, {
		description: "Total active time",
		unit: "s",
	});

	return {
		recordSessionStarted(startType) {
			sessions.add(1, { start_type: startType });
		},
		recordSessionEnded() {},
		recordModelUsage({ model, tokens: counts, costUsd }) {
			for (const [type, count] of tokenEntries(counts, "cacheCreation")) {
				addWhenPositive(tokens, count, { type, model });
			}
			addWhenPositive(cost, costUsd, { model });
		},
		recordInteraction({ durationSeconds }) {
			addWhenPositive(activeTime, durationSeconds, { type: "cli" });
		},
		recordToolExecution() {},
	};
}

export function createMetricsRecorder(config: MetricsConfig, meter: MetricsMeter): MetricsRecorder {
	return config.schema === "claude_code"
		? createClaudeCodeMetricsRecorder(meter, config.prefix)
		: createPiMetricsRecorder(meter, config.prefix);
}
