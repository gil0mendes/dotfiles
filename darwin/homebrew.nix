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
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
    "homebrew/cask-versions"
    "homebrew/services"
  ];

  homebrew.brews = [
    "jq"
    "python3"
  ] ++ (if isWork then
    [
      "yarn"
      "git-lfs"
      "shellcheck"
      "quilt"
      "watchman"
      "coreutils"
      "helm"
      "openlens"
    ]
  else
    [ ]
  );

  homebrew.casks = [
    # productivity
    "firefox"
    "libreoffice"
    "notion"
    "transmission"
    "1password"
    "thunderbird"
    "inkscape"
    "vlc"

    # media
    "spotify"

    # development
    "fork"
    "visual-studio-code"
    "iterm2"
    "postman"
    "robo-3t"
    "docker"
    "virtualbox"
    "dbeaver-community"
    "yubico-yubikey-manager"
  ]
  ++ (if isWork then
    [ ]
  else
    [
      # productivity
      "google-chrome"

      # Social
      "telegram"
      "whatsapp"
    ]);
}
