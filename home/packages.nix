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
    neovim
    openssl
    python3
    qemu
    tmux
    tmuxinator
    tree

    ## rust packages
    rustup
    exa # fancy version of `ls`
    tealdeer # rust implementation of `tldr`

    # Useful nix related tools
    nixpkgs-fmt
    comma # run software from without installing it
  ];
}
