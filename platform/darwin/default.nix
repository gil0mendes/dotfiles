{ pkgs, lib, ... }:

{
  imports = [
    # Minimal config of Nix related options and shells
    ./bootstrap.nix

    # Other nix-darwin configuration
    ./homebrew.nix
  ];

  # Networking configurations
  networking.dns = [
    "1.1.1.1"
    "8.8.8.8"
  ];

  # Fonts
  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    recursive
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];

  # Keyboard
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;

  # Add ability to used TouchID for sudo authentication
  security.pam.enableSudoTouchIdAuth = true;
}
