{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  home.file.".pi".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/pi";

  home.sessionVariables.BUN_INSTALL = "$HOME/.bun";
  home.sessionPath = [ "$HOME/.bun/bin" ];

  programs.fish.shellInit = lib.mkAfter ''
    set -gx BUN_INSTALL "$HOME/.bun"
    fish_add_path "$BUN_INSTALL/bin"
  '';

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
