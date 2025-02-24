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
    devenv
  ];

  services.kanata = {
    enable = true;
    config = ''
      (defcfg
        process-unmapped-keys yes
        macos-dev-names-include (
          "Apple Internal Keyboard / Trackpad"
          "Keychron K6"
        )
      )

      (defsrc 
        esc	f1	f2	f3	f4	f5	f6	f7	f8	f9	f10	f11	f12
        caps a s d f j k l ;
        fn
      )

      (defvar
        tap-time 150
        hold-time 200
      )

      (defalias
        escctrl (tap-hold 100 100 esc lctl)
        a (tap-hold $tap-time $hold-time a lmet)
        s (tap-hold $tap-time $hold-time s lalt)
        d (tap-hold $tap-time $hold-time d lsft)
        f (tap-hold $tap-time $hold-time f lctl)
        j (tap-hold $tap-time $hold-time j rctl)
        k (tap-hold $tap-time $hold-time k rsft)
        l (tap-hold $tap-time $hold-time l ralt)
        ; (tap-hold $tap-time $hold-time ; rmet)
        fnl (tap-hold 200 200 fn (layer-toggle fn))
      )

      (deflayer base
        esc	brdn	brup	_	_	_	_	prev pp	next	mute	vold	volu
        @escctrl @a @s @d @f @j @k @l @;
        @fnl
      )

      (deflayer fn
        esc	f1	f2	f3	f4	f5	f6	f7	f8	f9	f10	f11	f12
        @escctrl _ _ _ _ _ _ _ _
        fn
      )
    '';
  };

  # Fonts
  fonts.packages = with pkgs; [
    recursive
    nerd-fonts.jetbrains-mono
  ];

  # Keyboard
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;

  # Add ability to used TouchID for sudo authentication
  security.pam.enableSudoTouchIdAuth = true;
}
