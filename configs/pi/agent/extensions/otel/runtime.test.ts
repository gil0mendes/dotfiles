import { afterEach, describe, expect, it, vi } from "vitest";
import type { MetricsConfig } from "./config";
import type { MetricsRecorder } from "./metrics";
import { acquireMetricsRuntime, type MetricsRuntime, type MetricsRuntimeLease } from "./runtime";

const CONFIG: MetricsConfig = {
	schema: "pi",
	prefix: "pi",
	protocol: "http/protobuf",
	temporality: "delta",
	exportIntervalMillis: 60_000,
	exportTimeoutMillis: 30_000,
};

function createFakeRuntime(shutdown: () => Promise<void> = async () => {}) {
	const recorder: MetricsRecorder = {
		recordSessionStarted: () => {},
		recordSessionEnded: () => {},
		recordModelUsage: () => {},
		recordInteraction: () => {},
		recordToolExecution: () => {},
	};
	const runtime: MetricsRuntime = { recorder, shutdown: vi.fn(shutdown) };
	return runtime;
}

describe("acquireMetricsRuntime", () => {
	const leases: MetricsRuntimeLease[] = [];

	async function acquire(factory: () => Promise<MetricsRuntime>, timeoutMillis?: number) {
		const lease = await acquireMetricsRuntime(CONFIG, factory, timeoutMillis);
		leases.push(lease);
		return lease;
	}

	afterEach(async () => {
		await Promise.all(leases.splice(0).map((lease) => lease.release()));
	});

	it("shares one runtime between concurrent sessions", async () => {
		const runtime = createFakeRuntime();
		const factory = vi.fn(async () => runtime);

		const [first, second] = await Promise.all([acquire(factory), acquire(factory)]);

		expect(factory).toHaveBeenCalledTimes(1);
		expect(first.recorder).toBe(second.recorder);
	});

	it("shuts down only when the last lease is released", async () => {
		const runtime = createFakeRuntime();
		const first = await acquire(async () => runtime);
		const second = await acquire(async () => runtime);

		expect(await first.release()).toBe("retained");
		expect(runtime.shutdown).not.toHaveBeenCalled();
		expect(await second.release()).toBe("flushed");
		expect(runtime.shutdown).toHaveBeenCalledTimes(1);
	});

	it("releases idempotently", async () => {
		const runtime = createFakeRuntime();
		const lease = await acquire(async () => runtime);
		const other = await acquire(async () => runtime);

		await lease.release();
		await lease.release();
		expect(runtime.shutdown).not.toHaveBeenCalled();

		expect(await other.release()).toBe("flushed");
		expect(await other.release()).toBe("retained");
		expect(runtime.shutdown).toHaveBeenCalledTimes(1);
	});

	it("creates a fresh runtime after the previous one was shut down", async () => {
		const factory = vi.fn(async () => createFakeRuntime());

		await (await acquire(factory)).release();
		await acquire(factory);

		expect(factory).toHaveBeenCalledTimes(2);
	});

	it("bounds shutdown time", async () => {
		const runtime = createFakeRuntime(() => new Promise<void>(() => {}));
		const lease = await acquire(async () => runtime, 10);

		expect(await lease.release()).toBe("timed_out");
	});

	it("reports a failed shutdown without throwing", async () => {
		const runtime = createFakeRuntime(async () => {
			throw new Error("export endpoint unreachable");
		});
		const lease = await acquire(async () => runtime);

		expect(await lease.release()).toBe("failed");
	});

	it("reports a shutdown that throws synchronously as failed", async () => {
		const runtime = createFakeRuntime();
		runtime.shutdown = vi.fn(() => {
			throw new Error("exporter already closed");
		});
		const lease = await acquire(async () => runtime);

		expect(await lease.release()).toBe("failed");
	});

	it("starts a new runtime for a session that begins while the last one is shutting down", async () => {
		const closing = createFakeRuntime(() => new Promise<void>(() => {}));
		const replacement = createFakeRuntime();
		const factory = vi.fn<() => Promise<MetricsRuntime>>().mockResolvedValueOnce(closing);
		factory.mockResolvedValueOnce(replacement);
		const first = await acquire(factory, 10);

		const releasing = first.release();
		const second = await acquire(factory);

		expect(second.recorder).toBe(replacement.recorder);
		expect(factory).toHaveBeenCalledTimes(2);
		expect(await releasing).toBe("timed_out");
	});

	it("does not cache a failed start", async () => {
		const failing = vi.fn(async (): Promise<MetricsRuntime> => {
			throw new Error("boom");
		});
		await expect(acquire(failing)).rejects.toThrow("boom");

		const runtime = createFakeRuntime();
		const lease = await acquire(async () => runtime);

		expect(lease.recorder).toBe(runtime.recorder);
	});
});
