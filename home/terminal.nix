{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  # TODO: build is broken in macOS - I'll use brew for now
  # programs.wezterm = {
  #   enable = true;
  # };
  xdg.configFile."wezterm".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/wezterm";
}
