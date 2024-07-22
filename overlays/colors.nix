# Colors from: https://ethanschoonover.com/solarized
# Used in Kitty terminal config: `./kitty-configs.nix`

final: prev: {
  lib = prev.lib // {
    colors = {
      solarized = rec {
        colors = {
          darkBase = "002b36"; # base03
          darkBasehl = "073642"; # base02
          darkestTone = "586e75"; # base01
          darkTone = "657b83"; # base00
          lightTone = "839496"; # base0
          lightestTone = "93a1a1"; # base1
          lightBasehl = "eee8d5"; # base2
          lightBase = "fdf6e3"; # base3
          yellow = "b58900";
          orange = "cb4b16";
          red = "dc322f";
          magenta = "d33682";
          violet = "6c71c4";
          blue = "268bd2";
          cyan = "2aa198";
          green = "859900";
        };

        light = with colors; {
          base = lightBase;
          basehl = lightBasehl;
          invbase = darkBase;
          invbasehl = darkBasehl;
          main = darkTone;
          faded = lightTone;
          muted = lightestTone;
          strong = darkestTone;
          inherit (colors) yellow orange red megenta violet blue cyan green;
        };

        dark = with colors; {
          base = darkBase;
          basehl = darkBasehl;
          invbase = lightBase;
          invbasehl = lightBasehl;
          main = lightTone;
          faded = darkTone;
          muted = darkestTone;
          strong = lightestTone;
          inherit (colors) yellow orange red megenta violet blue cyan green;
        };
      };
			catppuccin = rec {
        colors = {
          darkBase = "24273a"; # base03
          darkBasehl = "073642"; # base02
          darkestTone = "586e75"; # base01
          darkTone = "4c4f69"; # base00
          lightTone = "cad3f5"; # base0
          lightestTone = "93a1a1"; # base1
          lightBasehl = "eee8d5"; # base2
          lightBase = "fdf6e3"; # base3
          yellow = "df8e1d";
          orange = "fe640b";
          red = "d20f39";
          magenta = "ea76cb";
          violet = "8839ef";
          blue = "1e66f5";
          cyan = "179299";
          green = "40a02b";
        };

        light = with colors; {
          base = "eff1f5";
          basehl = lightBasehl;
          invbase = darkBase;
          invbasehl = darkBasehl;
          main = "4c4f69";
          faded = lightTone;
          muted = "dc8a78";
          strong = darkestTone;
          darkBase = "6c6f85"; # base03
          darkBasehl = "5c5f77"; # base02
          darkestTone = "40a02b"; # base01
          darkTone = "df8e1d"; # base00
          lightTone = "1e66f5"; # base0
          lightestTone = "179299"; # base1
          lightBasehl = "acb0be"; # base2
          lightBase = "bcc0cc"; # base3
          yellow = "df8e1d";
          orange = "d20f39";
          red = "d20f39";
          magenta = "ea76cb";
          violet = "ea76cb";
          blue = "1e66f5";
          cyan = "179299";
          green = "40a02b";
        };

        dark = with colors; {
          base = "24273a";
          basehl = "181926";
          invbase = lightBase;
          invbasehl = lightBasehl;
          main = "cad3f5";
          faded = darkTone;
          muted = "f4dbd6";
          strong = lightestTone;
          darkBase = "5b6078"; # base03
          darkBasehl = "494d64"; # base02
          darkestTone = "a6da95"; # base01
          darkTone = "eed49f"; # base00
          lightTone = "8aadf4"; # base0
          lightestTone = "8bd5ca"; # base1
          lightBasehl = "b8c0e0"; # base2
          lightBase = "a5adcb"; # base3
          yellow = "eed49f";
          orange = "ed8796";
          red = "ed8796";
          magenta = "f5bde6";
          violet = "f5bde6";
          blue = "8aadf4";
          cyan = "8bd5ca";
          green = "a6da95";
        };
			};
    };
  };
}
