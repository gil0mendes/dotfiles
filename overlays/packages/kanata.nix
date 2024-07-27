{ lib, stdenv, fetchurl, ... }:

stdenv.mkDerivation {
  pname = "myKanata";
  version = "1.7.0-prerelease-1";

  src = fetchurl {
    url = "https://github.com/jtroo/kanata/releases/download/v1.7.0-prerelease-1/kanata_macos_arm64";
    hash = "sha256-y3ZD9ygB8SSTVpL9e2MqDfkEocB+J/EYhgUEiGLr3SM=";
  };

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
          		mkdir -p $out/bin
    					# chmod a+x $src
          		cp $src $out/bin/kanata
          		'';
}
