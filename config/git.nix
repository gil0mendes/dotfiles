{
  enable = true;
  userName = "Gil Mendes";
  userEmail = "gil00mendes@gmail.com";
  ignores = [ "*~" "*.swp" ];
  extraConfig = {
    init.defaultBranch = "main";

    core.editor = "vim";
    # For supercede
    core.symlinks = true;
    
    credential.helper = "osxkeychain";
    pull.rebase = "true";
  };
}
