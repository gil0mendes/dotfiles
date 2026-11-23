import { describe, expect, it } from "vitest";
import { type Environment, parseOtelConfig } from "./config";

const ENABLED: Environment = { PI_OTEL_ENABLED: "true" };

function parseEnabled(overrides: Environment = {}) {
	const result = parseOtelConfig({ ...ENABLED, ...overrides });
	if (result.status !== "enabled")
		throw new Error(`expected enabled, got ${JSON.stringify(result)}`);
	return result.metrics;
}

function parseInvalid(overrides: Environment): string {
	const result = parseOtelConfig({ ...ENABLED, ...overrides });
	if (result.status !== "invalid") throw new Error(`expected invalid, got ${result.status}`);
	return result.message;
}

describe("parseOtelConfig", () => {
	it("is disabled unless PI_OTEL_ENABLED opts in", () => {
		expect(parseOtelConfig({})).toEqual({
			status: "disabled",
			reason: "PI_OTEL_ENABLED is not set",
		});
		expect(parseOtelConfig({ PI_OTEL_ENABLED: "false" }).status).toBe("disabled");
	});

	it("does not validate other settings while disabled", () => {
		expect(parseOtelConfig({ PI_OTEL_METRICS_PREFIX: "!!" }).status).toBe("disabled");
	});

	it("honours the standard kill switches", () => {
		expect(parseOtelConfig({ ...ENABLED, OTEL_SDK_DISABLED: "true" }).status).toBe("disabled");
		expect(parseOtelConfig({ ...ENABLED, OTEL_METRICS_EXPORTER: "none" }).status).toBe("disabled");
	});

	it("applies defaults for the pi schema", () => {
		expect(parseEnabled()).toEqual({
			schema: "pi",
			prefix: "pi",
			protocol: "http/protobuf",
			temporality: "delta",
			exportIntervalMillis: 60_000,
			exportTimeoutMillis: 30_000,
		});
	});

	it("defaults the prefix to the schema name", () => {
		expect(parseEnabled({ PI_OTEL_METRICS_SCHEMA: "claude_code" })).toMatchObject({
			schema: "claude_code",
			prefix: "claude_code",
		});
	});

	it("normalizes trailing dots in the prefix and lets it override the schema default", () => {
		expect(parseEnabled({ PI_OTEL_METRICS_PREFIX: " acme.pi.. " }).prefix).toBe("acme.pi");
		expect(
			parseEnabled({ PI_OTEL_METRICS_SCHEMA: "claude_code", PI_OTEL_METRICS_PREFIX: "acme" })
				.prefix,
		).toBe("acme");
	});

	it("treats a blank prefix as unset", () => {
		expect(parseEnabled({ PI_OTEL_METRICS_PREFIX: "  " }).prefix).toBe("pi");
	});

	it.each([".", "9pi", "pi..usage", "pi usage", "a".repeat(65)])("rejects prefix %j", (prefix) => {
		expect(parseInvalid({ PI_OTEL_METRICS_PREFIX: prefix })).toContain("PI_OTEL_METRICS_PREFIX");
	});

	it("rejects unknown schemas", () => {
		expect(parseInvalid({ PI_OTEL_METRICS_SCHEMA: "datadog" })).toContain("pi, claude_code");
	});

	it("prefers the per-signal protocol and rejects grpc", () => {
		expect(
			parseEnabled({
				OTEL_EXPORTER_OTLP_PROTOCOL: "http/protobuf",
				OTEL_EXPORTER_OTLP_METRICS_PROTOCOL: "http/json",
			}).protocol,
		).toBe("http/json");
		expect(parseInvalid({ OTEL_EXPORTER_OTLP_PROTOCOL: "grpc" })).toContain(
			"grpc is not supported",
		);
	});

	it("rejects exporters other than otlp", () => {
		expect(parseEnabled({ OTEL_METRICS_EXPORTER: "otlp" }).schema).toBe("pi");
		expect(parseInvalid({ OTEL_METRICS_EXPORTER: "console,otlp" })).toContain(
			"OTEL_METRICS_EXPORTER",
		);
	});

	it("validates endpoints without echoing their value", () => {
		const message = parseInvalid({ OTEL_EXPORTER_OTLP_ENDPOINT: "ftp://user:secret@host" });
		expect(message).toContain("OTEL_EXPORTER_OTLP_ENDPOINT");
		expect(message).not.toContain("secret");
		expect(parseInvalid({ OTEL_EXPORTER_OTLP_METRICS_ENDPOINT: "not a url" })).toContain(
			"OTEL_EXPORTER_OTLP_METRICS_ENDPOINT",
		);
		expect(
			parseEnabled({ OTEL_EXPORTER_OTLP_ENDPOINT: "https://collector.example:4318" }),
		).toBeTruthy();
	});

	it("parses temporality case-insensitively", () => {
		expect(
			parseEnabled({ OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE: "Cumulative" }).temporality,
		).toBe("cumulative");
		expect(parseInvalid({ OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE: "weekly" })).toContain(
			"delta, cumulative, lowmemory",
		);
	});

	describe("export timing", () => {
		it("clamps the default timeout to a shorter interval", () => {
			expect(parseEnabled({ OTEL_METRIC_EXPORT_INTERVAL: "5000" })).toMatchObject({
				exportIntervalMillis: 5_000,
				exportTimeoutMillis: 5_000,
			});
		});

		it("rejects an explicit timeout longer than an explicit interval", () => {
			expect(
				parseInvalid({ OTEL_METRIC_EXPORT_INTERVAL: "1000", OTEL_METRIC_EXPORT_TIMEOUT: "2000" }),
			).toContain("must not exceed");
		});

		it("rejects an explicit timeout longer than the default interval", () => {
			expect(parseInvalid({ OTEL_METRIC_EXPORT_TIMEOUT: "90000" })).toContain("60000 ms");
			expect(parseEnabled({ OTEL_METRIC_EXPORT_TIMEOUT: "60000" }).exportTimeoutMillis).toBe(
				60_000,
			);
		});

		it.each(["0", "-5", "1.5", "soon", "0x10", "1e3", "2147483648"])(
			"rejects interval %j",
			(value) => {
				expect(parseInvalid({ OTEL_METRIC_EXPORT_INTERVAL: value })).toContain(
					"OTEL_METRIC_EXPORT_INTERVAL",
				);
			},
		);

		it("accepts the largest interval a Node timer can honour", () => {
			expect(parseEnabled({ OTEL_METRIC_EXPORT_INTERVAL: "2147483647" }).exportIntervalMillis).toBe(
				2_147_483_647,
			);
		});
	});

	describe("exporter headers", () => {
		it("accepts percent-encoded pairs, metadata and stray commas", () => {
			expect(
				parseEnabled({
					OTEL_EXPORTER_OTLP_HEADERS: "authorization=Bearer%20abc, x-team=pi;ttl=1,",
					OTEL_EXPORTER_OTLP_METRICS_HEADERS: "x-api-key=secret",
				}),
			).toBeTruthy();
		});

		it.each(["authorization", "=value", "x-key=", "bad name=1", "x=%ZZ", "x=a%0Ab", "x=%E2%82%AC"])(
			"rejects header entry %j",
			(entry) => {
				const message = parseInvalid({
					OTEL_EXPORTER_OTLP_METRICS_HEADERS: `authorization=Bearer%20ok,${entry}`,
				});
				expect(message).toContain("OTEL_EXPORTER_OTLP_METRICS_HEADERS");
			},
		);

		it("never echoes credentials", () => {
			const message = parseInvalid({
				OTEL_EXPORTER_OTLP_HEADERS: "authorization=Bearer%20hunter2,broken-hunter2",
			});
			expect(message).not.toContain("hunter2");
		});

		it("validates the generic variable too", () => {
			expect(parseInvalid({ OTEL_EXPORTER_OTLP_HEADERS: "oops" })).toContain(
				"OTEL_EXPORTER_OTLP_HEADERS",
			);
		});
	});

	describe("resource attributes", () => {
		it("accepts percent-encoded pairs, empty values and stray commas", () => {
			expect(
				parseEnabled({ OTEL_RESOURCE_ATTRIBUTES: "team=platform,note=a%2Cb, empty=," }),
			).toBeTruthy();
		});

		it.each(["team", "a=b=c", "=x", "k=%E0%A4%A", `k=${"v".repeat(256)}`])(
			"rejects malformed attributes %#",
			(entry) => {
				expect(parseInvalid({ OTEL_RESOURCE_ATTRIBUTES: `team=platform,${entry}` })).toContain(
					"OTEL_RESOURCE_ATTRIBUTES",
				);
			},
		);
	});

	it("rejects malformed booleans instead of guessing", () => {
		expect(parseOtelConfig({ PI_OTEL_ENABLED: "maybe" })).toEqual({
			status: "invalid",
			message: "PI_OTEL_ENABLED must be a boolean (true/false)",
		});
	});
});
