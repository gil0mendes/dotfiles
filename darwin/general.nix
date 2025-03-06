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
        tap-time 200
        hold-time 150

        left-hand-keys (
          q w e r t
          a s d f g
          z x c v b
        )

        right-hand-keys (
          y u i o p
          h j k l ;
          n m , . /
        )
      )

      (deffakekeys
        to-base (layer-switch base)
      )
        
      (defalias
        tap (multi
          (layer-switch nomods)
          (on-idle-fakekey to-base tap 20)
        )

        escctrl (tap-hold 100 100 esc lctl)
        a (tap-hold-release-keys $tap-time $hold-time (multi a @tap) lmet $left-hand-keys)
        s (tap-hold-release-keys $tap-time $hold-time (multi s @tap) lalt $left-hand-keys)
        d (tap-hold-release-keys $tap-time $hold-time (multi d @tap) lsft $left-hand-keys)
        f (tap-hold-release-keys $tap-time $hold-time (multi f @tap) lctl $left-hand-keys)
        j (tap-hold-release-keys $tap-time $hold-time (multi j @tap) rctl $right-hand-keys)
        k (tap-hold-release-keys $tap-time $hold-time (multi k @tap) rsft $right-hand-keys)
        l (tap-hold-release-keys $tap-time $hold-time (multi l @tap) ralt $right-hand-keys)
        ; (tap-hold-release-keys $tap-time $hold-time (multi ; @tap) rmet $right-hand-keys)
        fnl (tap-hold 200 200 fn (layer-toggle fn))
      )

      (deflayer base
        esc	brdn	brup	_	_	_	_	prev pp	next	mute	vold	volu
        @escctrl @a @s @d @f @j @k @l @;
        @fnl
      )

      (deflayer nomods
        esc	f1	f2	f3	f4	f5	f6	f7	f8	f9	f10	f11	f12
        caps a s d f j k l ;
        fn
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
	security.pam.services.sudo_local.touchIdAuth = true;
}
