{ pkgs, lib, config, ... }:

{
  programs.gpg.enable = true;

  home.file.".gnupg/gpg-agent.conf".text = ''
    max-cache-ttl 18000
    default-cache-ttl 18000
    pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
    enable-ssh-support
  '';
}
