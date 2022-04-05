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

  homebrew.masApps = (if isWork then
    { } else
    {
      Keynote = 409183694;
      Numbers = 409203825;
      Pages = 409201541;
    }
  );

  # NOTE: this is only required for work
  homebrew.brews = [ ] ++ (if isWork then
    [
      "aria2"
      "jq"
      "yarn"
      "git-lfs"
      "python3"
      "shellcheck"
      "quilt"
      "watchman"
      "yarn"
      "coreutils"
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
    "emacs"

    # social & media
    "spotify"
    "telegram"
    "whatsapp"

    # development
    "fork"
    "visual-studio-code"
    "iterm2"
    "postman"
    "robo-3t"
    "docker"
  ]
  ++ (if isWork then
    [ ]
  else
    [
      "google-chrome"
      # TODO: move into the main set of modules when the sha256 is fixed on the homebrew repo
      "dbeaver-community"
    ]);
}
