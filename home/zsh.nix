{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  # ZSH Shell
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.zsh.enable
  programs.zsh.enable = true;

  # --- Config {{{
  xdg.configFile."zsh".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/zsh";
  programs.zsh.envExtra = ''
    if [[ -z "$XDG_CONFIG_HOME" ]] then
      export XDG_CONFIG_HOME="$HOME/.config/"
    fi

    if [[ -d "$XDG_CONFIG_HOME/zsh" ]] then
      export ZDOTDIR="$XDG_CONFIG_HOME/zsh/"
    fi
  '';
  # }}}
}
