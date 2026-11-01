{ config, ... }:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  home.file.".agents/skills".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/agents/skills";
}
