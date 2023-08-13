{ config, pkgs, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-gtk3;
    extraConfig = "(package-initialize) (org-babel-load-file \" ~/.config/emacs/config.org \")";
  };

  xdg.configFile."emacs".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/emacs";
}
