{ config, ... }:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  xdg.configFile."opencode".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/opencode";
}
