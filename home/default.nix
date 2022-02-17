{ config, pkgs, ... }:

{
  # Import config broken out into files
  imports = [
    ./git.nix
  ];

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
    exa
    tldr
  ];

  # Starship Prompt
  # TODO: disbaled for now, there is a build error happening
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.starship.enable
  programs.starship.enable = false;

  programs.starship.settings = {
    # See docs here: https://starship.rs/config/
    # Symbols config configured in Flake.

    battery.display.threshold = 25; # display battery information if charge is <= 25%
    directory.fish_style_pwd_dir_length = 1; # turn on fish directory truncation
    directory.truncation_length = 2; # number of directories not to truncate
    gcloud.disabled = true; # annoying to always have on
    hostname.style = "bold green"; # don't like the default
    memory_usage.disabled = true; # because it includes cached memory it's reported as full a lot
    username.style_user = "bold blue"; # don't like the default
  };

  # This value determines the Home Manager release that your configuration is compatible with. This
  # helps avoid breakage when a new Home Manager release introduces backwards incompatible changes.
  #
  # You can update Home Manager without changing this value. See the Home Manager release notes for
  # a list of state version changes in each release.
  home.stateVersion = "22.05";
}
