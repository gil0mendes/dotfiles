import { describe, expect, it } from "vitest";
import type { MetricsRecorder } from "./metrics";
import { createSessionObserver } from "./observer";

function createRecordingObserver() {
	let currentTime = 0;
	const calls: Array<{ method: string; argument?: unknown }> = [];
	const recorder: MetricsRecorder = {
		recordSessionStarted: (argument) => void calls.push({ method: "sessionStarted", argument }),
		recordSessionEnded: (argument) => void calls.push({ method: "sessionEnded", argument }),
		recordModelUsage: (argument) => void calls.push({ method: "modelUsage", argument }),
		recordInteraction: (argument) => void calls.push({ method: "interaction", argument }),
		recordToolExecution: (argument) => void calls.push({ method: "tool", argument }),
	};
	const observer = createSessionObserver({ recorder, now: () => currentTime });
	return {
		observer,
		calls,
		advanceSeconds: (seconds: number) => {
			currentTime += seconds * 1000;
		},
	};
}

function assistantMessage(overrides: Record<string, unknown> = {}) {
	return {
		role: "assistant",
		content: [{ type: "text", text: "a secret answer" }],
		provider: "anthropic",
		model: "claude-sonnet-5",
		stopReason: "stop",
		usage: {
			input: 10,
			output: 20,
			cacheRead: 30,
			cacheWrite: 40,
			reasoning: 15,
			totalTokens: 100,
			cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0.5 },
		},
		...overrides,
	};
}

describe("session observer", () => {
	it("records usage once per finalized assistant message", () => {
		const { observer, calls } = createRecordingObserver();
		const message = assistantMessage();

		observer.messageEnded(message);
		observer.messageEnded(message);

		expect(calls).toEqual([
			{
				method: "modelUsage",
				argument: {
					model: "claude-sonnet-5",
					provider: "anthropic",
					tokens: { input: 10, output: 20, cacheRead: 30, cacheWrite: 40 },
					costUsd: 0.5,
					costSource: "estimate",
				},
			},
		]);
	});

	it("does not add reasoning tokens on top of output", () => {
		const { observer, calls } = createRecordingObserver();
		observer.messageEnded(assistantMessage());

		expect(JSON.stringify(calls)).not.toContain("reasoning");
		expect(calls[0]?.argument).toMatchObject({ tokens: { output: 20 } });
	});

	it("flags provider-reported cost", () => {
		const { observer, calls } = createRecordingObserver();
		observer.messageEnded(
			assistantMessage({
				usage: { input: 1, output: 1, cost: { total: 0.1, totalFromProvider: true } },
			}),
		);

		expect(calls[0]?.argument).toMatchObject({ costUsd: 0.1, costSource: "provider" });
	});

	it("never forwards message content", () => {
		const { observer, calls } = createRecordingObserver();
		observer.messageEnded(assistantMessage());

		expect(JSON.stringify(calls)).not.toContain("secret");
	});

	it.each([
		["a user message", { role: "user", content: "hi" }],
		["a streaming message", assistantMessage({ stopReason: "pending" })],
		["a deferred message", assistantMessage({ stopReason: "deferred" })],
		["a message without a model", assistantMessage({ model: "" })],
		["a message without provider", assistantMessage({ provider: undefined })],
		["a message without usage", assistantMessage({ usage: undefined })],
	])("ignores %s", (_label, message) => {
		const { observer, calls } = createRecordingObserver();
		observer.messageEnded(message);

		expect(calls).toEqual([]);
	});

	it("ignores non-message input", () => {
		const { observer, calls } = createRecordingObserver();
		observer.messageEnded(undefined);
		observer.messageEnded("text");

		expect(calls).toEqual([]);
	});

	it("sanitizes malformed numbers to zero", () => {
		const { observer, calls } = createRecordingObserver();
		observer.messageEnded(
			assistantMessage({
				usage: { input: -5, output: Number.NaN, cacheRead: "7", cost: { total: Infinity } },
			}),
		);

		expect(calls[0]?.argument).toMatchObject({
			tokens: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
			costUsd: 0,
		});
	});

	it("measures one interaction across several agent starts until the run settles", () => {
		const { observer, calls, advanceSeconds } = createRecordingObserver();

		observer.agentStarted();
		advanceSeconds(2);
		observer.agentStarted();
		advanceSeconds(3);
		observer.agentSettled(false);
		observer.agentSettled(false);

		expect(calls).toEqual([
			{ method: "interaction", argument: { durationSeconds: 5, aborted: false } },
		]);
	});

	it("ignores a settle that has no matching start", () => {
		const { observer, calls } = createRecordingObserver();
		observer.agentSettled(true);

		expect(calls).toEqual([]);
	});

	it("records tool outcome, nesting and only valid durations", () => {
		const { observer, calls } = createRecordingObserver();

		observer.toolExecutionEnded({ toolName: "bash", isError: true, durationMs: 1500 });
		observer.toolExecutionEnded({ toolName: "read", isError: false, parentToolCallId: "call-1" });
		observer.toolExecutionEnded({ toolName: "edit", isError: false, durationMs: -1 });

		expect(calls.map(({ argument }) => argument)).toEqual([
			{ toolName: "bash", succeeded: false, nested: false, durationSeconds: 1.5 },
			{ toolName: "read", succeeded: true, nested: true, durationSeconds: undefined },
			{ toolName: "edit", succeeded: true, nested: false, durationSeconds: undefined },
		]);
	});

	it("counts a session once and records its duration when it ends", () => {
		const { observer, calls, advanceSeconds } = createRecordingObserver();

		observer.sessionStarted("fresh");
		advanceSeconds(30);
		observer.sessionEnded();
		observer.sessionEnded();

		expect(calls).toEqual([
			{ method: "sessionStarted", argument: "fresh" },
			{ method: "sessionEnded", argument: 30 },
		]);
	});

	it("keeps the time of a run that was still going when the session ended", () => {
		const { observer, calls, advanceSeconds } = createRecordingObserver();

		observer.sessionStarted("fresh");
		observer.agentStarted();
		advanceSeconds(4);
		observer.sessionEnded();
		observer.agentSettled(false);

		expect(calls).toEqual([
			{ method: "sessionStarted", argument: "fresh" },
			{ method: "interaction", argument: { durationSeconds: 4, aborted: true } },
			{ method: "sessionEnded", argument: 4 },
		]);
	});

	it("records no session duration when it never saw the session start", () => {
		const { observer, calls, advanceSeconds } = createRecordingObserver();

		advanceSeconds(10);
		observer.sessionEnded();

		expect(calls).toEqual([]);
	});
});
