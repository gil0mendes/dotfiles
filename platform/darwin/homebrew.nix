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

  homebrew.casks = [
    # productivity
    "firefox"
    "google-chrome"
    "libreoffice"
    "notion"
    "transmission"
    "1password"
    "slack"

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
  ];
}
