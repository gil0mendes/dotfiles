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
  config = {
    xdg.configFile."opencode".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/opencode";

    home.sessionVariables = lib.optionalAttrs isWork {
      OPENCODE_CONFIG = "${nixConfigDirectory}/configs/opencode/opencode.work.json";
    };

    programs.fish.interactiveShellInit = lib.mkAfter (
      lib.optionalString isWork ''
        set -l opencodeOtlpIdentity "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt"
        set -l opencodeOtlpEndpointFile "${nixConfigDirectory}/home/secrets/opencode-otlp-endpoint.age"
        set -l opencodeOtlpHeadersFile "${nixConfigDirectory}/home/secrets/opencode-otlp-headers.age"

        set -l opencodeOtlpEndpoint (${pkgs.age}/bin/age --decrypt --identity "$opencodeOtlpIdentity" "$opencodeOtlpEndpointFile" 2>/dev/null)
        set -l opencodeOtlpEndpointStatus $status
        set -l opencodeOtlpHeaders (${pkgs.age}/bin/age --decrypt --identity "$opencodeOtlpIdentity" "$opencodeOtlpHeadersFile" 2>/dev/null)
        set -l opencodeOtlpHeadersStatus $status

        if test $opencodeOtlpEndpointStatus -ne 0; or test $opencodeOtlpHeadersStatus -ne 0; or test (count $opencodeOtlpEndpoint) -ne 1; or test (count $opencodeOtlpHeaders) -ne 1; or test -z "$opencodeOtlpEndpoint"; or test -z "$opencodeOtlpHeaders"
          set --erase OPENCODE_OTLP_ENDPOINT OPENCODE_OTLP_HEADERS
          printf '%s\n' 'warning: OpenCode OTLP environment not loaded' >&2
        else
          set --global --export OPENCODE_OTLP_ENDPOINT "$opencodeOtlpEndpoint"
          set --global --export OPENCODE_OTLP_HEADERS "$opencodeOtlpHeaders"
        end
      ''
    );
  };
}
