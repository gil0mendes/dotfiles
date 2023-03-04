{
  description = "Gil's darwin system";

  inputs = {
    # Package sets
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;

    # Environment/system management
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # Other sources
    moses-lua = { url = github:Yonaba/Moses; flake = false; };
  };

  outputs = inputs @ { self, darwin, nixpkgs, home-manager, ... }:
    let
      inherit (darwin.lib) darwinSystem;
      inherit (inputs.nixpkgs-unstable.lib) attrValues makeOverridable optionalAttrs singleton;

      # Configuration for `nixpkgs`
      nixpkgsConfig = {
        config = { allowUnfree = true; };
        overlays = attrValues self.overlays ++ singleton (
          # Sub in x86 version of packages that don't build on Apple Silicon yet
          final: prev: (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            inherit (final.pkgs-x86)
              nix-index
              starship; # TODO: remove when https://github.com/NixOS/nixpkgs/issues/160876 is fixed.
          })
        );
      };

      homeManagerStateVersion = "22.05";

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
        # My Apple Silicon macOS laptop config
        g0m = darwinSystem {
          system = "aarch64-darwin";
          modules = nixDarwinCommonModules ++ [
            {
              users.primaryUser = primaryUserInfo;

              networking.computerName = "Gil 💻";
              networking.hostName = "GilBookPro";
              networking.knownNetworkServices = [
                "Wi-Fi"
              ];
            }
          ];
        };

        # My Personal MacBook Pro from 2015
        pmbp = darwinSystem {
          system = "x86_64-darwin";
          modules = nixDarwinCommonModules ++ [
            {
              users.primaryUser = primaryUserInfo;

              networking.computerName = "Personal Gil 💻";
              networking.hostName = "PersonalGilBookPro";
              networking.knownNetworkServices = [
                "Wi-Fi"
              ];
            }
          ];
        };

        # My Work MacBook Pro from 2019
        work = darwinSystem {
          system = "x86_64-darwin";
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
        # Add access to x86 packages if the system is running Apple Silicon
        apple-silicon = final: prev: optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
          pkgs-x86 = import inputs.nixpkgs-unstable {
            system = "x86_64-darwin";
            inherit (nixpkgsConfig) config;
          };
        };

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
      };

      darwinModules = {
        # My configurations
        g0m-bootstrap = import ./darwin/bootstrap.nix;
        g0m-defaults = import ./darwin/defaults.nix;
        g0m-general = import ./darwin/general.nix;
        g0m-homebrew = import ./darwin/homebrew.nix;

        # Modules pending upstream
        security-pam = import ./modules/darwin/security/pam.nix;
        users-primaryUser = import ./modules/darwin/users.nix;
      };

      homeManagerModules = {
        # My configurations
        g0m-fish = import ./home/fish.nix;
        g0m-git = import ./home/git.nix;
        g0m-gpg = import ./home/gpg.nix;
        g0m-kitty = import ./home/kitty.nix;
        g0m-neovim = import ./home/neovim.nix;
        g0m-packages = import ./home/packages.nix;
        g0m-starship = import ./home/starship.nix;
        g0m-starship-symbols = import ./home/starship-symbols.nix;
        g0m-zsh = import ./home/zsh.nix;

        programs-kitty-extras = import ./modules/home/programs/kitty/extras.nix;
        programs-neovim-extras = import ./modules/home/programs/neovim/extras.nix;
        home-user-info = { lib, ... }: {
          options.home.user-info = (self.darwinModules.users-primaryUser { inherit lib; }).options.users.primaryUser;
        };
      };
    };
}
