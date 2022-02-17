{ pkgs, lib, ... }:

{
  programs.git = {
    enable = true;

    userEmail = "gil00mendes@gmail.com";
    userName = "Gil Mendes";

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
      credential.helper = "osxkeychain";
      pull.rebase = "true";
      # For supercede
      core.symlinks = true;
    };
  };
}
