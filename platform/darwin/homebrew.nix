{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  mkIfCaskPresent = cask: mkIf (lib.any (x: x == cask) config.homebrew.casks);
  brewEnabled = config.homebrew.enable;
in

{
  environment.shellInit = mkIf brewEnabled ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '';

  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
    "homebrew/cask-versions"
    "homebrew/core"
    "homebrew/services"
  ];

  homebrew.masApps = {
    Keynote = 409183694;
    Numbers = 409203825;
    Pages = 409201541;
  };

  homebrew.casks = [
    # productivity
    "firefox"
    "google-chrome"
    "libreoffice"
    "notion"
    "transmission"
    "1password"
    "slack"
    "emacs"

    # social & media
    "spotify"
    "telegram"
    "whatsapp"

    # development
    "dbeaver-community"
    "fork"
    "visual-studio-code"
    "iterm2"
    "postman"
    "robo-3t"
    "docker"
  ];

  # For cli packages that aren't currently available for macOS in `nixpkgs` or have errors. Packages should be installed 
  # in `../home/default.nix` whenever possible.
  homebrew.brews = [
    "starship"
  ];
}
