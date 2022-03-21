{ pkgs, lib, config, ... }:

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
    };
  };
}
