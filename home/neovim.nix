{ config, pkgs, lib, ... }:

let
  inherit (lib) getName mkIf optional;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  # Neovim
  # https://nix-community.github.io/home-manager/options.xhtml#opt-programs.neovim.enable
  programs.neovim = {
    enable = true;
    withNodeJs = true;
    withPython3 = true;
    viAlias = true;
    vimAlias = true;
  };

  home.shellAliases = {
    v = "nvim";
  };
}
