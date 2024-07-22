{ config, pkgs, ... }:

let
  inherit (config.users) primaryUser;
in
{

  # Bat, a substitute for cat.
  # https://github.com/sharkdp/bat
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bat.enable
  programs.bat.enable = true;
  programs.bat.config = {
    style = "plain";
  };
  programs.bat.themes = {
    catppuccin_latte = {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "d714cc1d358ea51bfc02550dabab693f70cccea0";
        sha256 = "sha256-Q5B4NDrfCIK3UAMs94vdXnR42k4AXCqZz6sRn8bzmf4=";
      };
      file = "themes/Catppuccin Latte.tmTheme";
    };
    catppuccin_macchiato = {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "d714cc1d358ea51bfc02550dabab693f70cccea0";
        sha256 = "sha256-Q5B4NDrfCIK3UAMs94vdXnR42k4AXCqZz6sRn8bzmf4=";
      };
      file = "themes/Catppuccin Macchiato.tmTheme";
    };
  };

  # Htop
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.htop.enable
  programs.htop.enable = true;
  programs.htop.settings.show_program_path = true;

  home.packages = with pkgs; [
    axel
    curl
    wget
    fzf
    gnupg
    openssl
    python3
    qemu
    tmux
    tmuxinator
    tree
    jq

    ## rust packages
    rustup
    eza # fancy version of `ls`
    tealdeer # rust implementation of `tldr`
    starship # shell prompt
    ripgrep

    # Useful nix related tools
    nixpkgs-fmt
    comma # run software from without installing it

    # LSP
    nixd
    nodePackages.typescript-language-server
    tree-sitter
    eslint_d

    # Yubikey
    yubikey-manager
  ];
}
