{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
  isWork = config.home.user-info.username == "gmendes";
in
{
  home.file.".pi".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/pi";

  home.sessionVariables.BUN_INSTALL = "$HOME/.bun";
  home.sessionPath = [ "$HOME/.bun/bin" ];

  programs.fish.shellInit = lib.mkAfter ''
    set -gx BUN_INSTALL "$HOME/.bun"
    fish_add_path "$BUN_INSTALL/bin"
  '';

  # Reuses the OpenCode OTLP secrets. Their endpoint is a collector base URL (the OpenCode plugin
  # appends /v1/<signal>), while OTEL_EXPORTER_OTLP_METRICS_ENDPOINT is used as is, so the metrics
  # URL is derived here. Metrics-specific variables keep other OpenTelemetry apps unaffected.
  programs.fish.interactiveShellInit = lib.mkAfter (
    lib.optionalString isWork ''
      set -l piOtelIdentity "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt"
      set -l piOtelEndpointFile "${nixConfigDirectory}/home/secrets/opencode-otlp-endpoint.age"
      set -l piOtelHeadersFile "${nixConfigDirectory}/home/secrets/opencode-otlp-headers.age"

      set -l piOtelBaseEndpoint (${pkgs.age}/bin/age --decrypt --identity "$piOtelIdentity" "$piOtelEndpointFile" 2>/dev/null)
      set -l piOtelEndpointStatus $status
      set -l piOtelHeaders (${pkgs.age}/bin/age --decrypt --identity "$piOtelIdentity" "$piOtelHeadersFile" 2>/dev/null)
      set -l piOtelHeadersStatus $status

      if test $piOtelEndpointStatus -ne 0; or test $piOtelHeadersStatus -ne 0; or test (count $piOtelBaseEndpoint) -ne 1; or test (count $piOtelHeaders) -ne 1; or test -z "$piOtelBaseEndpoint"; or test -z "$piOtelHeaders"; or string match --quiet --regex '[?#]' -- "$piOtelBaseEndpoint"
        set --global --export PI_OTEL_ENABLED false
        set --global --erase PI_OTEL_METRICS_SCHEMA PI_OTEL_METRICS_PREFIX OTEL_EXPORTER_OTLP_METRICS_PROTOCOL OTEL_EXPORTER_OTLP_METRICS_ENDPOINT OTEL_EXPORTER_OTLP_METRICS_HEADERS
        printf '%s\n' 'warning: Pi OTLP environment not loaded' >&2
      else
        set -l piOtelMetricsEndpoint (string replace --regex '/+$' "" -- "$piOtelBaseEndpoint")"/v1/metrics"

        set --global --export PI_OTEL_ENABLED true
        set --global --export PI_OTEL_METRICS_SCHEMA claude_code
        set --global --export PI_OTEL_METRICS_PREFIX claude_code
        set --global --export OTEL_EXPORTER_OTLP_METRICS_PROTOCOL http/json
        set --global --export OTEL_EXPORTER_OTLP_METRICS_ENDPOINT "$piOtelMetricsEndpoint"
        set --global --export OTEL_EXPORTER_OTLP_METRICS_HEADERS "$piOtelHeaders"
      end
    ''
  );

  programs.zsh.envExtra = lib.mkAfter ''
    export BUN_INSTALL="$HOME/.bun"
    export PATH="$BUN_INSTALL/bin:$PATH"
  '';

  home.activation.installPiCodingAgent = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    export BUN_INSTALL="${config.home.homeDirectory}/.bun"
    mkdir -p "$BUN_INSTALL/bin"
    ${pkgs.bun}/bin/bun add -g @earendil-works/pi-coding-agent
  '';
}
