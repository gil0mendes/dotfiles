{ config, pkgs, ... }:

let
  inherit (config.users) primaryUser;
in
{

  # Bat, a substitute for cat.
  # https://github.com/sharkdp/bat
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bat.enable
  programs.bat.enable = true;
  programs.bat.config = {
    style = "plain";
  };
  programs.bat.themes = {
    catppuccin_latte = {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "d714cc1d358ea51bfc02550dabab693f70cccea0";
        sha256 = "sha256-Q5B4NDrfCIK3UAMs94vdXnR42k4AXCqZz6sRn8bzmf4=";
      };
      file = "themes/Catppuccin Latte.tmTheme";
    };
    catppuccin_macchiato = {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "d714cc1d358ea51bfc02550dabab693f70cccea0";
        sha256 = "sha256-Q5B4NDrfCIK3UAMs94vdXnR42k4AXCqZz6sRn8bzmf4=";
      };
      file = "themes/Catppuccin Macchiato.tmTheme";
    };
    # TODO: add rose-pine themes
  };

  # Htop
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.htop.enable
  programs.htop.enable = true;
  programs.htop.settings.show_program_path = true;

  home.packages = with pkgs; [
    # base tools
    coreutils
    axel
    curl
    wget
    fzf
    gnupg
    openssl
    python3
    qemu
    tmux
    tmuxinator
    tree
    jq
    yq
    kubernetes-helm
    vscode
    kubectl
    kubelogin
    sops
    age
    oras
    atuin

    # clouds tools
    awscli2
    azure-cli

    # modern and fast alternative to find
    fd
    # tools to work with images
    imagemagick_light
    # tools to work with PDF
    ghostscript
    # tool to render mermaid diagrams
    mermaid-cli
    lua51Packages.luarocks_bootstrap

    ## rust packages
    rustup
    eza # fancy version of `ls`
    tealdeer # rust implementation of `tldr`
    starship # shell prompt
    ripgrep

    # Useful nix related tools
    comma # run software from without installing it

    tree-sitter

    # Yubikey
    yubikey-manager

    # LSP and Linting tools
    ## Markdown and other doc types
    marksman
    pandoc
    ## NIX
    nixfmt-rfc-style
    nil
    ## JS/TS
    nodePackages.typescript-language-server
    eslint_d
    ## JSON
    vscode-langservers-extracted
    ## Lua
    lua-language-server
    stylua
    ## YAML
    yaml-language-server
  ];
}
