import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import otelExtension from "./index";
import type { MetricsRecorder } from "./metrics";
import type { MetricsRuntime } from "./runtime";
import { createSdkMetricsRuntime } from "./sdk-runtime";

vi.mock("./sdk-runtime", () => ({ createSdkMetricsRuntime: vi.fn() }));

type Handler = (event: unknown, ctx: ExtensionContext) => unknown;

function createHarness() {
	const handlers = new Map<string, Handler>();
	otelExtension({
		on: (name: string, handler: Handler) => handlers.set(name, handler),
	} as unknown as ExtensionAPI);

	const notifications: string[] = [];
	const ctx = {
		hasUI: true,
		ui: { notify: (message: string) => notifications.push(message) },
		sessionManager: { getEntries: () => [] },
	} as unknown as ExtensionContext;

	return {
		notifications,
		emit: (name: string, event: unknown = {}, context: ExtensionContext = ctx) =>
			handlers.get(name)?.(event, context),
	};
}

function createFakeRuntime() {
	const recorder = {
		recordSessionStarted: vi.fn(),
		recordSessionEnded: vi.fn(),
		recordModelUsage: vi.fn(),
		recordInteraction: vi.fn(),
		recordToolExecution: vi.fn(),
	} satisfies MetricsRecorder;
	const runtime: MetricsRuntime = { recorder, shutdown: vi.fn(async () => {}) };
	return { recorder, runtime };
}

describe("otel extension", () => {
	beforeEach(() => {
		vi.stubEnv("PI_OTEL_ENABLED", "true");
	});

	afterEach(() => {
		vi.unstubAllEnvs();
	});

	it("starts nothing while telemetry is not enabled", async () => {
		vi.stubEnv("PI_OTEL_ENABLED", "");
		const { emit } = createHarness();

		await emit("session_start", { reason: "startup" });

		expect(createSdkMetricsRuntime).not.toHaveBeenCalled();
	});

	it("reports invalid configuration and stays inert", async () => {
		vi.stubEnv("PI_OTEL_METRICS_PREFIX", "9bad");
		const { emit, notifications } = createHarness();

		await emit("session_start", { reason: "startup" });
		await emit("agent_start");

		expect(notifications).toHaveLength(1);
		expect(notifications[0]).toContain("PI_OTEL_METRICS_PREFIX");
		expect(createSdkMetricsRuntime).not.toHaveBeenCalled();
	});

	it("reports exporter startup failures without leaking the error message", async () => {
		vi.mocked(createSdkMetricsRuntime).mockRejectedValue(
			new TypeError("cannot reach https://user:secret@collector"),
		);
		const { emit, notifications } = createHarness();

		await emit("session_start", { reason: "startup" });

		expect(notifications).toHaveLength(1);
		expect(notifications[0]).toContain("TypeError");
		expect(notifications[0]).not.toContain("secret");
	});

	it("records a session from live events and flushes on shutdown", async () => {
		const { recorder, runtime } = createFakeRuntime();
		vi.mocked(createSdkMetricsRuntime).mockResolvedValue(runtime);
		const { emit } = createHarness();

		await emit("session_start", { reason: "startup" });
		await emit("agent_start");
		await emit("message_end", {
			message: {
				role: "assistant",
				provider: "anthropic",
				model: "claude-sonnet-5",
				stopReason: "stop",
				usage: { input: 1, output: 2, cost: { total: 0.01 } },
			},
		});
		await emit("tool_execution_end", { toolName: "bash", isError: false, durationMs: 250 });
		await emit("agent_settled", { aborted: false });
		await emit("session_shutdown", { reason: "quit" });

		expect(recorder.recordSessionStarted).toHaveBeenCalledWith("fresh");
		expect(recorder.recordModelUsage).toHaveBeenCalledTimes(1);
		expect(recorder.recordToolExecution).toHaveBeenCalledWith({
			toolName: "bash",
			succeeded: true,
			nested: false,
			durationSeconds: 0.25,
		});
		expect(recorder.recordInteraction).toHaveBeenCalledTimes(1);
		expect(recorder.recordSessionEnded).toHaveBeenCalledTimes(1);
		expect(runtime.shutdown).toHaveBeenCalledTimes(1);
	});

	it("classifies a startup that already has messages as a resume", async () => {
		const { recorder, runtime } = createFakeRuntime();
		vi.mocked(createSdkMetricsRuntime).mockResolvedValue(runtime);
		const { emit } = createHarness();
		const resumedContext = {
			hasUI: true,
			ui: { notify: () => {} },
			sessionManager: { getEntries: () => [{ type: "message" }] },
		} as unknown as ExtensionContext;

		await emit("session_start", { reason: "startup" }, resumedContext);
		await emit("session_shutdown", { reason: "quit" });

		expect(recorder.recordSessionStarted).toHaveBeenCalledWith("resume");
	});

	it("neither counts nor ends a session across /reload", async () => {
		const { recorder, runtime } = createFakeRuntime();
		vi.mocked(createSdkMetricsRuntime).mockResolvedValue(runtime);
		const { emit } = createHarness();

		await emit("session_start", { reason: "reload" });
		await emit("session_shutdown", { reason: "reload" });

		expect(recorder.recordSessionStarted).not.toHaveBeenCalled();
		expect(recorder.recordSessionEnded).not.toHaveBeenCalled();
		expect(runtime.shutdown).toHaveBeenCalledTimes(1);
	});

	it("releases the runtime when shutdown arrives while the exporter is still starting", async () => {
		const { runtime } = createFakeRuntime();
		let finishStartup: (started: MetricsRuntime) => void = () => {};
		vi.mocked(createSdkMetricsRuntime).mockReturnValue(
			new Promise<MetricsRuntime>((resolve) => {
				finishStartup = resolve;
			}),
		);
		const { emit } = createHarness();

		const starting = emit("session_start", { reason: "startup" });
		const shuttingDown = emit("session_shutdown", { reason: "quit" });
		finishStartup(runtime);
		await Promise.all([starting, shuttingDown]);

		expect(runtime.shutdown).toHaveBeenCalledTimes(1);
	});

	it("shares one runtime between in-process sessions", async () => {
		const { runtime } = createFakeRuntime();
		vi.mocked(createSdkMetricsRuntime).mockResolvedValue(runtime);
		const parent = createHarness();
		const child = createHarness();

		await parent.emit("session_start", { reason: "startup" });
		await child.emit("session_start", { reason: "startup" });
		await child.emit("session_shutdown", { reason: "quit" });
		expect(runtime.shutdown).not.toHaveBeenCalled();

		await parent.emit("session_shutdown", { reason: "quit" });
		expect(createSdkMetricsRuntime).toHaveBeenCalledTimes(1);
		expect(runtime.shutdown).toHaveBeenCalledTimes(1);
	});
});
