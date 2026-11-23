import type {
	ExtensionAPI,
	ExtensionContext,
	SessionStartEvent,
} from "@earendil-works/pi-coding-agent";
import { parseOtelConfig } from "./config";
import type { SessionStartType } from "./metrics";
import { createSessionObserver, type SessionObserver } from "./observer";
import { acquireMetricsRuntime, type MetricsRuntimeLease } from "./runtime";
import { createSdkMetricsRuntime } from "./sdk-runtime";

interface ActiveTelemetry {
	observer: SessionObserver;
	lease: MetricsRuntimeLease;
}

function reportProblem(ctx: ExtensionContext, message: string): void {
	const text = `OpenTelemetry metrics disabled: ${message}`;
	if (ctx.hasUI) ctx.ui.notify(text, "warning");
	else process.stderr.write(`${text}\n`);
}

// Only the error name is reported for SDK failures: their messages can embed endpoints or paths.
function describeStartupFailure(error: unknown): string {
	const name = error instanceof Error ? error.name : "unknown error";
	return `the exporter could not start (${name}); check the OTEL_* settings`;
}

function classifyStartup(
	reason: Exclude<SessionStartEvent["reason"], "reload">,
	ctx: ExtensionContext,
): SessionStartType {
	if (reason === "resume") return "resume";
	if (reason !== "startup") return "fresh";
	const hasEarlierMessages = ctx.sessionManager
		.getEntries()
		.some((entry) => entry.type === "message");
	return hasEarlierMessages ? "resume" : "fresh";
}

/**
 * Passive OpenTelemetry metrics for Pi. Opt-in through `PI_OTEL_ENABLED`; see README.md.
 * Nothing is loaded or started until `session_start` finds telemetry enabled.
 */
export default function otelExtension(pi: ExtensionAPI): void {
	let active: ActiveTelemetry | undefined;
	let startup: Promise<void> | undefined;

	/** Never rejects: telemetry problems are reported, not thrown into the session. */
	async function startTelemetry(
		event: Pick<SessionStartEvent, "reason">,
		ctx: ExtensionContext,
	): Promise<void> {
		const config = parseOtelConfig(process.env);
		if (config.status === "disabled") return;
		if (config.status === "invalid") {
			reportProblem(ctx, config.message);
			return;
		}

		try {
			const lease = await acquireMetricsRuntime(config.metrics, createSdkMetricsRuntime);
			const observer = createSessionObserver({ recorder: lease.recorder });
			// A reload re-creates the extension mid-session; that session was already counted, and its
			// true start is unknown, so it records no duration either.
			if (event.reason !== "reload") observer.sessionStarted(classifyStartup(event.reason, ctx));
			active = { observer, lease };
		} catch (error) {
			reportProblem(ctx, describeStartupFailure(error));
		}
	}

	pi.on("session_start", async (event, ctx) => {
		if (active !== undefined || startup !== undefined) return;
		startup = startTelemetry(event, ctx);
		try {
			await startup;
		} finally {
			startup = undefined;
		}
	});

	pi.on("agent_start", () => {
		active?.observer.agentStarted();
	});

	pi.on("agent_settled", (event) => {
		active?.observer.agentSettled(event.aborted);
	});

	pi.on("message_end", (event) => {
		active?.observer.messageEnded(event.message);
	});

	pi.on("tool_execution_end", (event) => {
		active?.observer.toolExecutionEnded(event);
	});

	pi.on("session_shutdown", async (event) => {
		// A shutdown that arrives while the exporter is still starting must not strand its lease.
		await startup;
		const finished = active;
		active = undefined;
		if (finished === undefined) return;
		if (event.reason !== "reload") finished.observer.sessionEnded();
		await finished.lease.release();
	});
}
