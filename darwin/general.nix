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
      				(defcfg
      								macos-dev-names-include (
      								 "Apple Internal Keyboard / Trackpad"
      								 "Keychron K6")
      				)
      				(defsrc 
      					esc	f1	f2	f3	f4	f5	f6	f7	f8	f9	f10	f11	f12
      					caps
      					fn
      				)
      				(deflayer base
      					esc	brdn	brup	_	_	_	_	prev pp	next	mute	vold	volu
      					@cap
      					@fnl
      				)
      				(deflayer fn
      					esc	f1	f2	f3	f4	f5	f6	f7	f8	f9	f10	f11	f12
      					_	
      					_
      				)
      				(defalias
      					cap (tap-hold-release 1 130 esc lctl)
      					fnl (tap-hold 200 200 fn (layer-toggle fn))
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
