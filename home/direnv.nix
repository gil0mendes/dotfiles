{ config, lib, pkgs, ... }:

{
  programs.direnv = {
    enable = true;

    # enable integrations
    enableBashIntegration = true;
    enableZshIntegration = true;

    nix-direnv.enable = true;
  };
}
