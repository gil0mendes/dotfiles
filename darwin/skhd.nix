{ config, lib, pkgs, ... }:
{
  services.skhd = {
    enable = true;
    skhdConfig = builtins.readFile ../configs/skhd.conf;
  };
}
