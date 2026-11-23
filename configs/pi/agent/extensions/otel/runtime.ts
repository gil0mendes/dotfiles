import type { MetricsConfig } from "./config";
import type { MetricsRecorder } from "./metrics";

export interface MetricsRuntime {
	readonly recorder: MetricsRecorder;
	/** Flushes pending metrics and releases exporter resources. */
	shutdown(): Promise<void>;
}

export type MetricsRuntimeFactory = (config: MetricsConfig) => Promise<MetricsRuntime>;

export type ReleaseOutcome = "retained" | "flushed" | "timed_out" | "failed";

export interface MetricsRuntimeLease {
	readonly recorder: MetricsRecorder;
	/** Idempotent. The last lease to be released shuts the runtime down within a bounded time. */
	release(): Promise<ReleaseOutcome>;
}

interface RuntimeEntry {
	readonly runtime: Promise<MetricsRuntime>;
	references: number;
}

interface RuntimeRegistry {
	entry: RuntimeEntry | undefined;
}

export const SHUTDOWN_TIMEOUT_MILLIS = 3_000;

// Pi may load this extension several times in one process (in-process agents, `/reload`), each
// time as a fresh module instance, so the registry lives on `globalThis`. The key is versioned
// because an older instance may still hold a registry shaped by previous code.
const REGISTRY_KEY = Symbol.for("pi.otel.metrics-runtime-registry.v1");

function isRuntimeRegistry(value: unknown): value is RuntimeRegistry {
	return typeof value === "object" && value !== null && "entry" in value;
}

function getRegistry(): RuntimeRegistry {
	const existing: unknown = Reflect.get(globalThis, REGISTRY_KEY);
	if (isRuntimeRegistry(existing)) return existing;
	const created: RuntimeRegistry = { entry: undefined };
	Reflect.set(globalThis, REGISTRY_KEY, created);
	return created;
}

function startEntry(
	registry: RuntimeRegistry,
	config: MetricsConfig,
	createRuntime: MetricsRuntimeFactory,
): RuntimeEntry {
	const entry: RuntimeEntry = { runtime: createRuntime(config), references: 0 };
	// A failed start must not poison the registry: the next session may retry.
	entry.runtime.catch(() => {
		if (registry.entry === entry) registry.entry = undefined;
	});
	registry.entry = entry;
	return entry;
}

async function shutdownWithin(
	runtime: MetricsRuntime,
	timeoutMillis: number,
): Promise<Exclude<ReleaseOutcome, "retained">> {
	let timer: ReturnType<typeof setTimeout> | undefined;
	const timedOut = new Promise<"timed_out">((resolve) => {
		timer = setTimeout(() => resolve("timed_out"), timeoutMillis);
		timer.unref();
	});
	// Wrapped so a shutdown that throws synchronously is a failed outcome, not a rejected release.
	const shutdown = (async () => runtime.shutdown())().then(
		() => "flushed" as const,
		() => "failed" as const,
	);
	try {
		return await Promise.race([shutdown, timedOut]);
	} finally {
		clearTimeout(timer);
	}
}

/**
 * Joins (or starts) the process-wide metrics runtime. The configuration of the first lease
 * wins: every session in a process reads the same environment.
 */
export async function acquireMetricsRuntime(
	config: MetricsConfig,
	createRuntime: MetricsRuntimeFactory,
	shutdownTimeoutMillis: number = SHUTDOWN_TIMEOUT_MILLIS,
): Promise<MetricsRuntimeLease> {
	const registry = getRegistry();
	const entry = registry.entry ?? startEntry(registry, config, createRuntime);
	entry.references += 1;

	let runtime: MetricsRuntime;
	try {
		runtime = await entry.runtime;
	} catch (error) {
		entry.references -= 1;
		throw error;
	}

	let released = false;
	return {
		recorder: runtime.recorder,
		async release() {
			if (released) return "retained";
			released = true;
			entry.references -= 1;
			if (entry.references > 0) return "retained";
			if (registry.entry === entry) registry.entry = undefined;
			return shutdownWithin(runtime, shutdownTimeoutMillis);
		},
	};
}
