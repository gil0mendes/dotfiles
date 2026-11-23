export type MetricsSchema = "pi" | "claude_code";
export type OtlpProtocol = "http/protobuf" | "http/json";
export type TemporalityPreference = "delta" | "cumulative" | "lowmemory";

export interface MetricsConfig {
	schema: MetricsSchema;
	prefix: string;
	protocol: OtlpProtocol;
	temporality: TemporalityPreference;
	exportIntervalMillis: number;
	exportTimeoutMillis: number;
}

export type OtelConfigResult =
	| { status: "disabled"; reason: string }
	| { status: "invalid"; message: string }
	| { status: "enabled"; metrics: MetricsConfig };

export type Environment = Readonly<Record<string, string | undefined>>;

const METRICS_SCHEMAS = ["pi", "claude_code"] as const satisfies readonly MetricsSchema[];
const OTLP_PROTOCOLS = ["http/protobuf", "http/json"] as const satisfies readonly OtlpProtocol[];
const TEMPORALITY_PREFERENCES = [
	"delta",
	"cumulative",
	"lowmemory",
] as const satisfies readonly TemporalityPreference[];

const DEFAULT_PREFIX_BY_SCHEMA: Record<MetricsSchema, string> = {
	pi: "pi",
	claude_code: "claude_code",
};
const DEFAULT_PROTOCOL: OtlpProtocol = "http/protobuf";
// Each Pi process exports under the same resource identity, so delta is the only temporality
// that keeps concurrent processes from looking like counter resets to the backend.
const DEFAULT_TEMPORALITY: TemporalityPreference = "delta";
const DEFAULT_EXPORT_INTERVAL_MILLIS = 60_000;
const DEFAULT_EXPORT_TIMEOUT_MILLIS = 30_000;
// Node timers clamp anything above this to 1 ms, which would turn the export timer into a busy loop.
const MAX_TIMER_MILLIS = 2 ** 31 - 1;
const MAX_PREFIX_LENGTH = 64;
const PREFIX_PATTERN = /^[A-Za-z][A-Za-z0-9_-]*(\.[A-Za-z0-9_-]+)*$/;
const ENDPOINT_VARIABLES = [
	"OTEL_EXPORTER_OTLP_ENDPOINT",
	"OTEL_EXPORTER_OTLP_METRICS_ENDPOINT",
] as const;
const HEADER_VARIABLES = [
	"OTEL_EXPORTER_OTLP_HEADERS",
	"OTEL_EXPORTER_OTLP_METRICS_HEADERS",
] as const;
const MAX_RESOURCE_ATTRIBUTE_LENGTH = 255;
const HTTP_HEADER_NAME_PATTERN = /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/;
// Visible ASCII, space, tab and Latin-1: the characters Node accepts in an outgoing header value.
const HTTP_HEADER_VALUE_PATTERN = /^[\t -~\u0080-\u00ff]*$/;

class OtelConfigError extends Error {}

function readSetting(env: Environment, name: string): string | undefined {
	const value = env[name]?.trim();
	return value ? value : undefined;
}

function readFirstSetting(
	env: Environment,
	names: readonly string[],
): [string, string] | undefined {
	for (const name of names) {
		const value = readSetting(env, name);
		if (value !== undefined) return [name, value];
	}
	return undefined;
}

function parseBoolean(name: string, value: string): boolean {
	const normalized = value.toLowerCase();
	if (["true", "1", "yes", "on"].includes(normalized)) return true;
	if (["false", "0", "no", "off"].includes(normalized)) return false;
	throw new OtelConfigError(`${name} must be a boolean (true/false)`);
}

function readBoolean(env: Environment, name: string): boolean {
	const value = readSetting(env, name);
	return value === undefined ? false : parseBoolean(name, value);
}

function parseChoice<Choice extends string>(
	name: string,
	value: string,
	choices: readonly Choice[],
): Choice {
	const match = choices.find((choice) => choice === value.toLowerCase());
	if (match !== undefined) return match;
	throw new OtelConfigError(`${name} must be one of: ${choices.join(", ")}`);
}

function parseTimerMilliseconds(name: string, value: string): number {
	const parsed = /^\d+$/.test(value) ? Number(value) : 0;
	if (parsed > 0 && parsed <= MAX_TIMER_MILLIS) return parsed;
	throw new OtelConfigError(
		`${name} must be a positive integer of milliseconds (at most ${MAX_TIMER_MILLIS})`,
	);
}

function isHttpUrl(value: string): boolean {
	if (!URL.canParse(value)) return false;
	const { protocol } = new URL(value);
	return protocol === "http:" || protocol === "https:";
}

// The value is never echoed: endpoints may carry credentials.
function assertEndpointsAreHttpUrls(env: Environment): void {
	for (const name of ENDPOINT_VARIABLES) {
		const value = readSetting(env, name);
		if (value !== undefined && !isHttpUrl(value)) {
			throw new OtelConfigError(`${name} must be an http(s) URL`);
		}
	}
}

function decodePercentEncoded(value: string): string | undefined {
	try {
		return decodeURIComponent(value);
	} catch {
		return undefined;
	}
}

/**
 * Mirrors the SDK's parser for `OTEL_EXPORTER_OTLP_*HEADERS` (W3C baggage format), which silently
 * drops entries it cannot read, so a typo would otherwise export without credentials. Also rejects
 * names and values Node would refuse to send.
 */
function isOtlpHeaderEntry(entry: string): boolean {
	const [pair = ""] = entry.split(";", 1);
	const separatorIndex = pair.indexOf("=");
	if (separatorIndex <= 0) return false;
	const rawName = pair.slice(0, separatorIndex).trim();
	const rawValue = pair.slice(separatorIndex + 1).trim();
	if (rawName === "" || rawValue === "") return false;
	const name = decodePercentEncoded(rawName);
	const value = decodePercentEncoded(rawValue);
	return (
		name !== undefined &&
		value !== undefined &&
		HTTP_HEADER_NAME_PATTERN.test(name) &&
		HTTP_HEADER_VALUE_PATTERN.test(value)
	);
}

// Neither names nor values are echoed: header entries typically carry credentials.
function assertHeadersAreWellFormed(env: Environment): void {
	for (const name of HEADER_VARIABLES) {
		const value = readSetting(env, name);
		if (value === undefined) continue;
		const entries = value.split(",").filter((entry) => entry.trim() !== "");
		if (!entries.every(isOtlpHeaderEntry)) {
			throw new OtelConfigError(
				`${name} must be comma-separated name=value pairs with percent-encoded special characters`,
			);
		}
	}
}

/**
 * Mirrors the SDK's parser for `OTEL_RESOURCE_ATTRIBUTES`, which discards the whole variable on
 * the first malformed entry; the metrics would then silently lose `service.name` and attribution.
 */
function isResourceAttributeEntry(entry: string): boolean {
	const parts = entry.split("=");
	if (parts.length !== 2) return false;
	const [rawKey = "", rawValue = ""] = parts;
	if (rawKey.trim() === "") return false;
	const key = decodePercentEncoded(rawKey.trim());
	const value = decodePercentEncoded(rawValue.trim());
	return (
		key !== undefined &&
		value !== undefined &&
		key.length <= MAX_RESOURCE_ATTRIBUTE_LENGTH &&
		value.length <= MAX_RESOURCE_ATTRIBUTE_LENGTH
	);
}

function assertResourceAttributesAreWellFormed(env: Environment): void {
	const value = readSetting(env, "OTEL_RESOURCE_ATTRIBUTES");
	if (value === undefined) return;
	const entries = value.split(",").filter((entry) => entry.trim() !== "");
	if (!entries.every(isResourceAttributeEntry)) {
		throw new OtelConfigError(
			`OTEL_RESOURCE_ATTRIBUTES must be comma-separated key=value pairs ` +
				`(at most ${MAX_RESOURCE_ATTRIBUTE_LENGTH} characters each) with "," and "=" percent-encoded`,
		);
	}
}

function isOtlpMetricsExporterSelected(env: Environment): boolean {
	const selected = (readSetting(env, "OTEL_METRICS_EXPORTER") ?? "otlp")
		.split(",")
		.map((exporter) => exporter.trim().toLowerCase())
		.filter((exporter) => exporter !== "");
	if (selected.length === 1 && selected[0] === "none") return false;
	if (selected.length > 0 && selected.every((exporter) => exporter === "otlp")) return true;
	throw new OtelConfigError('OTEL_METRICS_EXPORTER supports only "otlp" or "none"');
}

function normalizeMetricsPrefix(rawPrefix: string): string {
	const prefix = rawPrefix.replace(/\.+$/, "");
	if (prefix.length === 0 || prefix.length > MAX_PREFIX_LENGTH || !PREFIX_PATTERN.test(prefix)) {
		throw new OtelConfigError(
			`PI_OTEL_METRICS_PREFIX must be 1-${MAX_PREFIX_LENGTH} characters of letters, digits, "_", "-" ` +
				'and "." separators, starting with a letter',
		);
	}
	return prefix;
}

function parseProtocol(env: Environment): OtlpProtocol {
	const setting = readFirstSetting(env, [
		"OTEL_EXPORTER_OTLP_METRICS_PROTOCOL",
		"OTEL_EXPORTER_OTLP_PROTOCOL",
	]);
	if (setting === undefined) return DEFAULT_PROTOCOL;
	const [name, value] = setting;
	if (value.toLowerCase() === "grpc") {
		throw new OtelConfigError(`${name}=grpc is not supported; use http/protobuf or http/json`);
	}
	return parseChoice(name, value, OTLP_PROTOCOLS);
}

function parseExportTiming(env: Environment): { interval: number; timeout: number } {
	const intervalSetting = readSetting(env, "OTEL_METRIC_EXPORT_INTERVAL");
	const timeoutSetting = readSetting(env, "OTEL_METRIC_EXPORT_TIMEOUT");
	const interval =
		intervalSetting === undefined
			? DEFAULT_EXPORT_INTERVAL_MILLIS
			: parseTimerMilliseconds("OTEL_METRIC_EXPORT_INTERVAL", intervalSetting);
	if (timeoutSetting === undefined) {
		return { interval, timeout: Math.min(DEFAULT_EXPORT_TIMEOUT_MILLIS, interval) };
	}
	const timeout = parseTimerMilliseconds("OTEL_METRIC_EXPORT_TIMEOUT", timeoutSetting);
	// The SDK rejects a timeout above the interval, including the default interval.
	if (timeout > interval) {
		throw new OtelConfigError(
			`OTEL_METRIC_EXPORT_TIMEOUT must not exceed OTEL_METRIC_EXPORT_INTERVAL (${interval} ms)`,
		);
	}
	return { interval, timeout };
}

function parseEnabledConfig(env: Environment): OtelConfigResult {
	if (!readBoolean(env, "PI_OTEL_ENABLED")) {
		return { status: "disabled", reason: "PI_OTEL_ENABLED is not set" };
	}
	if (readBoolean(env, "OTEL_SDK_DISABLED")) {
		return { status: "disabled", reason: "OTEL_SDK_DISABLED is set" };
	}
	if (!isOtlpMetricsExporterSelected(env)) {
		return { status: "disabled", reason: "OTEL_METRICS_EXPORTER is none" };
	}
	assertEndpointsAreHttpUrls(env);
	assertHeadersAreWellFormed(env);
	assertResourceAttributesAreWellFormed(env);

	const schemaSetting = readSetting(env, "PI_OTEL_METRICS_SCHEMA");
	const schema =
		schemaSetting === undefined
			? "pi"
			: parseChoice("PI_OTEL_METRICS_SCHEMA", schemaSetting, METRICS_SCHEMAS);
	const prefixSetting = readSetting(env, "PI_OTEL_METRICS_PREFIX");
	const temporalitySetting = readSetting(env, "OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE");
	const { interval, timeout } = parseExportTiming(env);

	return {
		status: "enabled",
		metrics: {
			schema,
			prefix: normalizeMetricsPrefix(prefixSetting ?? DEFAULT_PREFIX_BY_SCHEMA[schema]),
			protocol: parseProtocol(env),
			temporality:
				temporalitySetting === undefined
					? DEFAULT_TEMPORALITY
					: parseChoice(
							"OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE",
							temporalitySetting,
							TEMPORALITY_PREFERENCES,
						),
			exportIntervalMillis: interval,
			exportTimeoutMillis: timeout,
		},
	};
}

/**
 * Parses the Pi/OpenTelemetry environment into trusted configuration.
 *
 * Endpoint, headers, timeout, compression and TLS settings are read from the standard
 * `OTEL_EXPORTER_OTLP_*` variables by the OTLP exporter itself, and resource attributes from
 * `OTEL_RESOURCE_ATTRIBUTES` by the SDK. Those readers silently ignore malformed input, so the
 * endpoint, header and resource variables are validated here: a typo fails loudly instead of
 * falling back to localhost, exporting without credentials or dropping `service.name`.
 */
export function parseOtelConfig(env: Environment): OtelConfigResult {
	try {
		return parseEnabledConfig(env);
	} catch (error) {
		if (error instanceof OtelConfigError) return { status: "invalid", message: error.message };
		throw error;
	}
}
