{ pkgs, lib, config, ... }:

let
  inherit (lib) mkIf;

  isWork = config.home.user-info.username == "gmendes";
in
{
  programs.git = {
    enable = true;

    userEmail = config.home.user-info.email;
    userName = config.home.user-info.fullName;

    # Enhanced diffs
    delta.enable = true;

    ignores = [
      "*~"
      "*.swp"
      ".DS_Store"
    ];

    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "vim";
      diff.colorMoved = "default";
      pull.rebase = true;
      # For supercede
      core.symlinks = true;
      # Automatically set upstream
      push.autoSetupRemote = true;
    };

    # PGP signing for work
    signing = {
      signByDefault = true;
    } // (if isWork then {
      key = "3900180E4467EA40BB5CC04411EC8E1407151F84";

    } else {
      key = "3C0FFA89D9EB7EF4BDABAB1B8108024DFE52031C";
    });
  };
}
