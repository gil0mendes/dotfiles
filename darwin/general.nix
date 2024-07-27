{ pkgs, lib, ... }:

{
  # Networking configurations
  networking.dns = [
    "1.1.1.1"
    "8.8.8.8"
  ];

  # Apps
  # `home-manager` currently has issues adding them to `~/Applications`
  # Issue: https://github.com/nix-community/home-manager/issues/1341
  environment.systemPackages = with pkgs; [
    kitty
    terminal-notifier
    emacs
  ];

  services.kanata = {
    enable = true;
    config = ''
      				(defsrc 
      				 caps
      				)
      				(deflayer base
      				 @cap
      				)
      				(defalias
      					cap (tap-hold-release 1 130 esc lctl)
      				)
      		'';
  };

  # Fonts
  fonts.packages = with pkgs; [
    recursive
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];

  # Keyboard
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;

  # Add ability to used TouchID for sudo authentication
  security.pam.enableSudoTouchIdAuth = true;
}
