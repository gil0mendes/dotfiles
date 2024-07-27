{
  description = "Gil's darwin system";

  inputs = {
    # Package sets
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # Allow to fix the CMD+Space search for isntalled applications
    mac-app-util.url = "github:hraban/mac-app-util";

    # Other sources
    moses-lua = { url = "github:Yonaba/Moses"; flake = false; };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs @ { self, darwin, nixpkgs, home-manager, mac-app-util, ... }:
    let
      inherit (darwin.lib) darwinSystem;
      inherit (inputs.nixpkgs-unstable.lib) attrValues optionalAttrs singleton;

      # Configuration for `nixpkgs`
      nixpkgsConfig = {
        config = { allowUnfree = true; };
        overlays = attrValues self.overlays;
      };

      homeManagerStateVersion = "24.05";

      workUserInfo = {
        username = "gmendes";
        fullName = "Gil Mendes";
        email = "gmendes@barracuda.com";
        nixConfigDirectory = "/Users/gmendes/.dotfiles";
      };

      primaryUserInfo = {
        username = "gil0mendes";
        fullName = "Gil Mendes";
        email = "gil00mendes@gmail.com";
        nixConfigDirectory = "/Users/gil0mendes/.dotfiles";
      };

      # Modules shared by most `nix-darwin` personal configurations.
      nixDarwinCommonModules = attrValues self.darwinModules ++ [
        # `home-manager` module
        home-manager.darwinModules.home-manager

        # mac-app-util modules
        mac-app-util.darwinModules.default

        (import ./modules/services/kanata.nix)

        (
          { config, lib, pkgs, ... }:
          let
            inherit (config.users) primaryUser;
          in
          {
            nixpkgs = nixpkgsConfig;


            # `home-manager` config
            users.users.${primaryUser.username}.home = "/Users/${primaryUser.username}";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${primaryUser.username} = {
              imports = attrValues self.homeManagerModules;
              home.stateVersion = homeManagerStateVersion;
              home.user-info = config.users.primaryUser;
            };

            # Add a registry entry for this flake
            nix.registry.my.flake = self;
          }
        )
      ];
    in
    {
      # My `nix-darwin` configs
      darwinConfigurations = rec {
        # Personal MacBook Pro from 2019
        "G0M-MBP" = darwinSystem {
          system = "x86_64-darwin";
          modules = nixDarwinCommonModules ++ [
            {
              users.primaryUser = primaryUserInfo;

              networking.computerName = "Gil Mendes 💻";
              networking.hostName = "G0M-MBP";
              networking.knownNetworkServices = [
                "Wi-Fi"
              ];
            }
          ];
        };

        # My Work MacBook Pro M3
        "ENG-GMENDES-MB" = darwinSystem {
          system = "aarch64-darwin";
          modules = nixDarwinCommonModules ++ [
            {
              users.primaryUser = workUserInfo;

              networking.computerName = "Gil Mendes Barracuda 💻";
              networking.hostName = "ENG-GMENDES-MB";
              networking.knownNetworkServices = [
                "Wi-Fi"
              ];
            }
          ];
        };
      };

      # --- Overlays

      overlays = {
        # load my customer packages
        customPkgs = import ./overlays/default.nix;

        # Overlay that adds various additional utility functions to `vimUtils`
        vimUtils = import ./overlays/vimUtils.nix;

        # Overlay that adds some additional Neovim plugins
        vimPlugins = final: prev:
          let
            inherit (self.overlays.vimUtils final prev) vimUtils;
          in
          {
            vimPlugins = prev.vimPlugins.extend (super: self:
              (vimUtils.buildVimPluginsFromFlakeInputs inputs [
                # Add plugins here
              ]) // {
                moses-nvim = vimUtils.buildNeovimLuaPackagePluginFromFlakeInput inputs "moses-lua";
              }
            );
          };

        # Overlay that adds `lib.colors` to reference colors elsewhere in system configs
        colors = import ./overlays/colors.nix;

        emacs = inputs.emacs.overlays.emacs;
      };

      darwinModules = {
        # My configurations
        g0m-bootstrap = import ./darwin/bootstrap.nix;
        g0m-defaults = import ./darwin/defaults.nix;
        g0m-general = import ./darwin/general.nix;
        g0m-homebrew = import ./darwin/homebrew.nix;

        # Modules pending upstream
        users-primaryUser = import ./modules/darwin/users.nix;
      };

      homeManagerModules = {
        # import mac-app-util for the user
        mac-app-util = mac-app-util.homeManagerModules.default;

        # My configurations
        g0m-emacs = import ./home/emacs.nix;
        g0m-fish = import ./home/fish.nix;
        g0m-git = import ./home/git.nix;
        g0m-gpg = import ./home/gpg.nix;
        g0m-kitty = import ./home/kitty.nix;
        g0m-neovim = import ./home/neovim.nix;
        g0m-packages = import ./home/packages.nix;
        g0m-starship = import ./home/starship.nix;
        g0m-starship-symbols = import ./home/starship-symbols.nix;
        g0m-zsh = import ./home/zsh.nix;
        g0m-direnv = import ./home/direnv.nix;

        programs-kitty-extras = import ./modules/home/programs/kitty/extras.nix;
        home-user-info = { lib, ... }: {
          options.home.user-info = (self.darwinModules.users-primaryUser { inherit lib; }).options.users.primaryUser;
        };
      };
    };
}
