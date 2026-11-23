import type { PushMetricExporter } from "@opentelemetry/sdk-metrics";
import type { MetricsConfig } from "./config";
import { createMetricsRecorder } from "./metrics";
import type { MetricsRuntime } from "./runtime";

const INSTRUMENTATION_SCOPE_NAME = "pi-otel-extension";
const DEFAULT_SERVICE_NAME = "pi";

// The OpenTelemetry SDK is imported lazily so sessions that leave telemetry disabled never pay
// for loading it.
async function createMetricExporter(config: MetricsConfig): Promise<PushMetricExporter> {
	const { AggregationTemporalityPreference, OTLPMetricExporter: JsonExporter } =
		await import("@opentelemetry/exporter-metrics-otlp-http");
	const temporalityPreference = {
		delta: AggregationTemporalityPreference.DELTA,
		cumulative: AggregationTemporalityPreference.CUMULATIVE,
		lowmemory: AggregationTemporalityPreference.LOWMEMORY,
	}[config.temporality];

	if (config.protocol === "http/json") return new JsonExporter({ temporalityPreference });
	const { OTLPMetricExporter: ProtobufExporter } =
		await import("@opentelemetry/exporter-metrics-otlp-proto");
	return new ProtobufExporter({ temporalityPreference });
}

async function createResource() {
	const {
		defaultResource,
		detectResources,
		resourceAttributesEnvDetector,
		resourceFromAttributes,
		serviceNameEnvDetector,
	} = await import("@opentelemetry/resources");
	// Later resources win: OTEL_SERVICE_NAME > OTEL_RESOURCE_ATTRIBUTES > "pi" > SDK default.
	return defaultResource()
		.merge(resourceFromAttributes({ "service.name": DEFAULT_SERVICE_NAME }))
		.merge(detectResources({ detectors: [resourceAttributesEnvDetector, serviceNameEnvDetector] }));
}

/**
 * Builds a dedicated, non-global MeterProvider that exports in the background on a timer, so
 * recording a metric never waits on the network. Endpoint, headers, timeout, compression and TLS
 * come from the standard `OTEL_EXPORTER_OTLP_*` variables read by the exporter.
 */
export async function createSdkMetricsRuntime(config: MetricsConfig): Promise<MetricsRuntime> {
	const { MeterProvider, PeriodicExportingMetricReader } =
		await import("@opentelemetry/sdk-metrics");
	const [exporter, resource] = await Promise.all([createMetricExporter(config), createResource()]);

	const reader = new PeriodicExportingMetricReader({
		exporter,
		exportIntervalMillis: config.exportIntervalMillis,
		exportTimeoutMillis: config.exportTimeoutMillis,
	});
	const meterProvider = new MeterProvider({ resource, readers: [reader] });

	return {
		recorder: createMetricsRecorder(config, meterProvider.getMeter(INSTRUMENTATION_SCOPE_NAME)),
		// Shutting the provider down performs the final flush before closing the exporter.
		shutdown: () => meterProvider.shutdown(),
	};
}
