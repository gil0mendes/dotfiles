{ config, lib, ... }:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
  isWork = config.home.user-info.username == "gmendes";
in
{
  xdg.configFile."opencode".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/opencode";

  home.sessionVariables = lib.optionalAttrs isWork {
    OPENCODE_CONFIG = "${nixConfigDirectory}/configs/opencode/opencode.work.json";
  };
}
