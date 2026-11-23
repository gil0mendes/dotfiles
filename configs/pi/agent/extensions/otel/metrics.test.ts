import { describe, expect, it } from "vitest";
import {
	createClaudeCodeMetricsRecorder,
	createMetricsRecorder,
	createPiMetricsRecorder,
	type MetricAttributes,
	type MetricsMeter,
	type ModelUsageSample,
} from "./metrics";

interface RecordedPoint {
	name: string;
	value: number;
	attributes: MetricAttributes | undefined;
}

function createFakeMeter() {
	const instruments = new Map<string, { kind: "counter" | "histogram"; unit?: string }>();
	const points: RecordedPoint[] = [];
	const meter: MetricsMeter = {
		createCounter(name, options) {
			instruments.set(name, { kind: "counter", unit: options.unit });
			return { add: (value, attributes) => void points.push({ name, value, attributes }) };
		},
		createHistogram(name, options) {
			instruments.set(name, { kind: "histogram", unit: options.unit });
			return { record: (value, attributes) => void points.push({ name, value, attributes }) };
		},
	};
	return { meter, instruments, points };
}

const USAGE: ModelUsageSample = {
	model: "claude-sonnet-5",
	provider: "anthropic",
	tokens: { input: 100, output: 40, cacheRead: 500, cacheWrite: 0 },
	costUsd: 0.0123,
	costSource: "provider",
};

describe("pi metrics schema", () => {
	it("creates prefixed instruments with units", () => {
		const { meter, instruments } = createFakeMeter();
		createPiMetricsRecorder(meter, "acme");

		expect([...instruments.keys()].sort()).toEqual([
			"acme.cost.usage",
			"acme.interaction.count",
			"acme.interaction.duration",
			"acme.session.count",
			"acme.session.duration",
			"acme.token.usage",
			"acme.tool.count",
			"acme.tool.duration",
		]);
		expect(instruments.get("acme.token.usage")).toEqual({ kind: "counter", unit: "tokens" });
		expect(instruments.get("acme.cost.usage")).toEqual({ kind: "counter", unit: "USD" });
		expect(instruments.get("acme.tool.duration")).toEqual({ kind: "histogram", unit: "s" });
	});

	it("records one token series per non-zero type with model and provider", () => {
		const { meter, points } = createFakeMeter();
		createPiMetricsRecorder(meter, "pi").recordModelUsage(USAGE);

		const attributes = { model: "claude-sonnet-5", provider: "anthropic" };
		expect(points).toEqual([
			{ name: "pi.token.usage", value: 100, attributes: { type: "input", ...attributes } },
			{ name: "pi.token.usage", value: 40, attributes: { type: "output", ...attributes } },
			{ name: "pi.token.usage", value: 500, attributes: { type: "cacheRead", ...attributes } },
			{
				name: "pi.cost.usage",
				value: 0.0123,
				attributes: { ...attributes, cost_source: "provider" },
			},
		]);
	});

	it("records sessions, interactions and tools", () => {
		const { meter, points } = createFakeMeter();
		const recorder = createPiMetricsRecorder(meter, "pi");

		recorder.recordSessionStarted("resume");
		recorder.recordSessionEnded(12);
		recorder.recordInteraction({ durationSeconds: 3, aborted: true });
		recorder.recordToolExecution({
			toolName: "bash",
			succeeded: false,
			nested: false,
			durationSeconds: 0.5,
		});
		recorder.recordToolExecution({
			toolName: "read",
			succeeded: true,
			nested: true,
			durationSeconds: undefined,
		});

		expect(points).toEqual([
			{ name: "pi.session.count", value: 1, attributes: { start_type: "resume" } },
			{ name: "pi.session.duration", value: 12, attributes: undefined },
			{ name: "pi.interaction.count", value: 1, attributes: { outcome: "aborted" } },
			{ name: "pi.interaction.duration", value: 3, attributes: { outcome: "aborted" } },
			{
				name: "pi.tool.count",
				value: 1,
				attributes: { tool_name: "bash", outcome: "error", nested: "false" },
			},
			{
				name: "pi.tool.duration",
				value: 0.5,
				attributes: { tool_name: "bash", outcome: "error", nested: "false" },
			},
			{
				name: "pi.tool.count",
				value: 1,
				attributes: { tool_name: "read", outcome: "success", nested: "true" },
			},
		]);
	});

	it("never attaches session identifiers", () => {
		const { meter, points } = createFakeMeter();
		const recorder = createPiMetricsRecorder(meter, "pi");
		recorder.recordSessionStarted("fresh");
		recorder.recordModelUsage(USAGE);

		const attributeKeys = points.flatMap(({ attributes }) => Object.keys(attributes ?? {}));
		expect(attributeKeys.some((key) => key.includes("session"))).toBe(false);
	});
});

describe("claude_code metrics schema", () => {
	it("only creates the metrics Pi can produce accurately", () => {
		const { meter, instruments } = createFakeMeter();
		createClaudeCodeMetricsRecorder(meter, "claude_code");

		expect([...instruments.keys()].sort()).toEqual([
			"claude_code.active_time.total",
			"claude_code.cost.usage",
			"claude_code.session.count",
			"claude_code.token.usage",
		]);
		expect(instruments.get("claude_code.active_time.total")).toEqual({
			kind: "counter",
			unit: "s",
		});
	});

	it("uses the documented token types and attributes", () => {
		const { meter, points } = createFakeMeter();
		createClaudeCodeMetricsRecorder(meter, "claude_code").recordModelUsage({
			...USAGE,
			tokens: { input: 1, output: 2, cacheRead: 3, cacheWrite: 4 },
		});

		expect(points.map(({ attributes }) => attributes?.type)).toEqual([
			"input",
			"output",
			"cacheRead",
			"cacheCreation",
			undefined,
		]);
		expect(points.every(({ attributes }) => attributes?.provider === undefined)).toBe(true);
		expect(points.at(-1)).toEqual({
			name: "claude_code.cost.usage",
			value: 0.0123,
			attributes: { model: "claude-sonnet-5" },
		});
	});

	it("maps interaction time to cli active time and ignores tools and session end", () => {
		const { meter, points } = createFakeMeter();
		const recorder = createClaudeCodeMetricsRecorder(meter, "claude_code");

		recorder.recordSessionStarted("fresh");
		recorder.recordInteraction({ durationSeconds: 2.5, aborted: false });
		recorder.recordToolExecution({
			toolName: "bash",
			succeeded: true,
			nested: false,
			durationSeconds: 1,
		});
		recorder.recordSessionEnded(60);

		expect(points).toEqual([
			{ name: "claude_code.session.count", value: 1, attributes: { start_type: "fresh" } },
			{ name: "claude_code.active_time.total", value: 2.5, attributes: { type: "cli" } },
		]);
	});
});

describe("createMetricsRecorder", () => {
	it("selects the recorder from the configured schema and prefix", () => {
		const { meter, instruments } = createFakeMeter();
		createMetricsRecorder(
			{
				schema: "claude_code",
				prefix: "acme",
				protocol: "http/protobuf",
				temporality: "delta",
				exportIntervalMillis: 1,
				exportTimeoutMillis: 1,
			},
			meter,
		);

		expect(instruments.has("acme.active_time.total")).toBe(true);
		expect(instruments.has("acme.tool.count")).toBe(false);
	});
});
