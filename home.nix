{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  # programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # User information.
  # home.username = "gil0mendes";
  # home.homeDirectory = "/Users/gil0mendes";

  home.packages = with pkgs; [
    axel
    curl
    wget
    fzf
    gnupg
    htop
    mas
    neovim
    openssl
    python3
    qemu
    tmux
    tmuxinator
    zsh

    ## rust packages
    rustup
    # exa
    # starship
    tldr
  ];

  programs = {
    git = import ./config/git.nix;
  };
}
