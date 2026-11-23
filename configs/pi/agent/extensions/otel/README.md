# otel

Passive OpenTelemetry **metrics** for Pi, exported over OTLP/HTTP. It only observes live events and never changes agent behaviour.

Off by default. Nothing is loaded or started until a session starts with `PI_OTEL_ENABLED=true`.

```sh
export PI_OTEL_ENABLED=true
export OTEL_EXPORTER_OTLP_ENDPOINT=https://collector.example.com:4318
export OTEL_EXPORTER_OTLP_HEADERS="authorization=Bearer ..."
pi
```

## Configuration

| Variable                                                     | Default           | Notes                                                                                                                                                        |
| ------------------------------------------------------------ | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `PI_OTEL_ENABLED`                                            | `false`           | Opt-in switch.                                                                                                                                               |
| `PI_OTEL_METRICS_SCHEMA`                                     | `pi`              | `pi` or `claude_code` (see below).                                                                                                                           |
| `PI_OTEL_METRICS_PREFIX`                                     | schema name       | Metric name prefix. Trailing dots are stripped (`acme.` → `acme`).                                                                                           |
| `OTEL_SDK_DISABLED`                                          | `false`           | Standard kill switch.                                                                                                                                        |
| `OTEL_METRICS_EXPORTER`                                      | `otlp`            | Only `otlp` or `none`.                                                                                                                                       |
| `OTEL_EXPORTER_OTLP_[METRICS_]PROTOCOL`                      | `http/protobuf`   | `http/protobuf` or `http/json`. `grpc` is rejected.                                                                                                          |
| `OTEL_EXPORTER_OTLP_[METRICS_]ENDPOINT`                      | SDK default       | Must be an http(s) URL. The generic variable is a base URL (`/v1/metrics` is appended); the metrics variable is used as is.                                  |
| `OTEL_EXPORTER_OTLP_[METRICS_]HEADERS`                       | none              | Comma-separated `name=value` pairs; percent-encode special characters. Timeout, compression and TLS are read by the exporter from `OTEL_EXPORTER_OTLP_*`.    |
| `OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE`          | `delta`           | `delta`, `cumulative` or `lowmemory`. Delta keeps concurrent Pi processes from looking like counter resets.                                                  |
| `OTEL_METRIC_EXPORT_INTERVAL` / `OTEL_METRIC_EXPORT_TIMEOUT` | `60000` / `30000` | Positive integer milliseconds, at most `2147483647`. A default timeout is clamped to a shorter interval; an explicit timeout above the interval is rejected. |
| `OTEL_SERVICE_NAME`, `OTEL_RESOURCE_ATTRIBUTES`              | `service.name=pi` | Resource attributes (use these for team or cost-centre attribution). `OTEL_RESOURCE_ATTRIBUTES` is `key=value` pairs with `,` and `=` percent-encoded.       |

Invalid values disable telemetry for the session and show one warning naming the variable. The OpenTelemetry SDK silently ignores malformed endpoints, headers and resource attributes (exporting to localhost, without credentials, or without `service.name`), so those are validated here instead. Endpoint, header and attribute values are never echoed.

## Metrics

Recording is in-memory; a background timer exports batches, so Pi never waits on the network. A final flush runs on exit and `/reload`, bounded to 3 s.

### `pi` schema (default)

With the default prefix `pi`:

| Metric                               | Type                | Unit     | Attributes                                                                 |
| ------------------------------------ | ------------------- | -------- | -------------------------------------------------------------------------- |
| `pi.session.count`                   | counter             |          | `start_type` (`fresh`, `resume`)                                           |
| `pi.session.duration`                | histogram           | `s`      |                                                                            |
| `pi.token.usage`                     | counter             | `tokens` | `type` (`input`, `output`, `cacheRead`, `cacheWrite`), `model`, `provider` |
| `pi.cost.usage`                      | counter             | `USD`    | `model`, `provider`, `cost_source` (`provider`, `estimate`)                |
| `pi.interaction.count` / `.duration` | counter / histogram | / `s`    | `outcome` (`completed`, `aborted`)                                         |
| `pi.tool.count` / `.duration`        | counter / histogram | / `s`    | `tool_name`, `outcome` (`success`, `error`), `nested` (`true`, `false`)    |

### `claude_code` schema

Emits the subset of the [Claude Code monitoring contract](https://code.claude.com/docs/en/monitoring-usage) that Pi can measure accurately, using the documented names, units and attribute values. The prefix defaults to `claude_code`.

| Metric                          | Attributes                                                        |
| ------------------------------- | ----------------------------------------------------------------- |
| `claude_code.session.count`     | `start_type` (`fresh`, `resume`)                                  |
| `claude_code.token.usage`       | `type` (`input`, `output`, `cacheRead`, `cacheCreation`), `model` |
| `claude_code.cost.usage`        | `model`                                                           |
| `claude_code.active_time.total` | `type=cli`                                                        |

Not emitted because Pi has no equivalent signal: `lines_of_code.count`, `pull_request.count`, `commit.count`, `code_edit_tool.decision`, `active_time.total{type=user}`, and the `query_source`, `speed`, `effort`, `agent.name`, `skill.name`, `plugin.name`, `mcp_*` attributes.

**Jellyfish compatibility is not verified.** The Jellyfish ingestion requirements were not available, so `claude_code` is based only on Claude Code's public documentation. `PI_OTEL_METRICS_PREFIX` only renames metrics; it does not make Jellyfish (or any backend) ingest them. Differences that could matter: no `session.id`, `user.*`, `organization.id` or `app.*` attributes are sent (Pi never puts identifiers in metric labels), and `start_type` has no `continue` value. Attribution can be supplied through `OTEL_RESOURCE_ATTRIBUTES`, which lands on the OTLP resource, not on data points. Use `PI_OTEL_METRICS_PREFIX` to align names with whatever the backend expects.

## Accuracy rules

- Tokens, cache and cost come from each finalized assistant message (`message_end`), once, using the `provider` and `model` on that message. Streaming/deferred messages and resumed history are never counted.
- `input` excludes cache tokens. Reasoning tokens are a subset of `output` and are not added or reported separately.
- Cost is Pi's `usage.cost.total`; `cost_source=provider` marks provider-reported cost, `estimate` marks cost computed from model pricing.
- An interaction is one agent run, from the first `agent_start` until `agent_settled` (retries and continuations included). Its duration is wall-clock time, including time blocked on UI prompts. A run still going when Pi quits is recorded as `aborted`.
- Tool calls made by another tool (for example a codemode script) are counted separately with `nested=true`; their time is also inside the caller's duration, so filter on `nested=false` when summing wall-clock time.
- Only operational metadata is recorded: no prompts, responses, tool arguments/results, headers, paths or error text. Session IDs are never metric attributes.

## Process model

Sessions in one process (in-process agents, `/reload`) share one reference-counted `MeterProvider`, created from the first session's configuration. It is not registered globally. The last session to shut down flushes and closes it. A session that was reloaded records no `session.duration` (its start is no longer known), and `/reload` does not count a new session.

## Limitations

- Only assistant-message usage is observed. Model usage that Pi books elsewhere (nested LLM calls reported on a tool result, compaction summaries, prompt-cache warming) is not exported, so tokens and cost can be lower than the totals Pi shows in its footer or `/session`.
- Traces and logs are deferred; only metrics are implemented.
- Export failures are silent (no diagnostics logger is installed) so the TUI stays clean.
- In-process subagents created through the SDK without `bindExtensions()` (as the `team` extension does) never emit `session_start`, so their usage is not observed.
- SDK embedders must tear sessions down through `AgentSessionRuntime.dispose()` (which emits `session_shutdown`); `session.dispose()` alone skips the final flush, leaving only the periodic export.
- `start_type=resume` is inferred when a session starts with existing messages; Pi cannot distinguish `--continue` from `--resume`.
- With `cumulative` temporality, concurrent Pi processes share one resource identity; add a unique `service.instance.id` through `OTEL_RESOURCE_ATTRIBUTES`.
