{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  mkIfCaskPresent = cask: mkIf (lib.any (x: x == cask) config.homebrew.casks);
  brewEnabled = config.homebrew.enable;

  isWork = config.users.primaryUser.username == "gmendes";
in

{
  environment.shellInit = mkIf brewEnabled ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '';

  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.lockfiles = true;

  homebrew.taps = [
    "homebrew/services"
  ];

  homebrew.casks = [
    # productivity
    "firefox"
    "libreoffice"
    "notion"
    "transmission"
    "1password"
    "inkscape"
    "vlc"

    # media
    "spotify"

    # development
    "fork"
    "postman"
    "docker"
    "dbeaver-community"
    "yubico-yubikey-manager"
  ]
  ++ (if isWork then
    [
      "openlens"
    ]
  else
    [
      # productivity
      "google-chrome"
      "thunderbird"
      "virtualbox"

      # development
      "robo-3t"

      # Social
      "telegram"
      "whatsapp"
    ]);
}
