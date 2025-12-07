{
  config,
  lib,
  pkgs,
  ...
}:

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
    "1password"
    "inkscape"
    "vlc"
    "karabiner-elements"
    "wezterm"
    "codex"

    # media
    "spotify"

    # development
    "fork"
    "postman"
    "dbeaver-community"
    "yubico-yubikey-manager"
    "zed"

    # Design
    "figma"
  ]
  ++ (
    if isWork then
      [
        "keepassx"
      ]
    else
      [
        # productivity
        "google-chrome"
        "thunderbird"
        "virtualbox"
        "transmission"

        # development
        "robo-3t"

        # intertainement
        "stremio"

        # Social
        "telegram"
        "whatsapp"
      ]
  );
  homebrew.masApps = {
    "Hidden Bar" = 1452453066;
  };
}
