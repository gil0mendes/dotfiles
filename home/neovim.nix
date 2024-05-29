{ config, pkgs, lib, ... }:

let
  inherit (lib) getName mkIf optional;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.home.user-info) nixConfigDirectory;

  pluginWithDeps = plugin: deps: plugin.overrideAttrs (_: { dependencies = deps; });

  nonVSCodePluginWithConfig = plugin: {
    plugin = plugin;
    optional = true;
    config = ''
      if !exists('g:vscode')
        lua require('g0m.' .. string.gsub('${plugin.pname}', '%.', '-'))
      endif
    '';
  };

  nonVSCodePlugin = plugin: {
    plugin = plugin;
    optional = true;
    config = ''if !exists('g:vscode') | packadd ${plugin.pname} | endif'';
  };
in
{
  # Neovim
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.neovim.enable
  programs.neovim.enable = true;

  # --- Config and plugins {{{
  # Minimal init.vim config to load Lua config. Nix and Home Manager don't currently support `init.lua`.
  xdg.configFile."nvim/lua".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/nvim/lua";
  xdg.configFile."nvim/colors".source = mkOutOfStoreSymlink "${nixConfigDirectory}/configs/nvim/colors";
  programs.neovim.extraConfig = "lua require('init')";

  programs.neovim.plugins = with pkgs.vimPlugins; [
    lush-nvim
    moses-nvim
    tabular
    vim-commentary
    vim-eunuch
    vim-haskell-module-name
    vim-surround
  ] ++ map (p: { plugin = p; optional = true; }) [
    which-key-nvim
  ] ++ map nonVSCodePlugin [
    direnv-vim
  ] ++ map nonVSCodePluginWithConfig [
    editorconfig-vim
    (pluginWithDeps galaxyline-nvim [ nvim-web-devicons ])
    gitsigns-nvim
    indent-blankline-nvim
    lspsaga-nvim
    (pluginWithDeps bufferline-nvim [ nvim-web-devicons ])
    nvim-cmp
    nvim-lspconfig
    nvim-treesitter
    (pluginWithDeps telescope-nvim [ nvim-web-devicons ])
    vim-floaterm
    vim-polyglot
  ];

  # From personal addon module `../modules/home/programs/neovim/extras.nix`
  programs.neovim.extras.termBufferAutoChangeDir = true;
  programs.neovim.extras.nvrAliases.enable = true;
  # }}}

  # --- Required packages {{{
  programs.neovim.extraPackages = with pkgs; [
    gcc # needed for nvim-treesitter
    tree-sitter # needed for nvim-treesitter

    # Language servers
    # See `../configs/nvim/lua/g0m/nvim-lspconfig.lua` for configuration.
    ccls
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.vim-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.yaml-language-server
  ] ++ optional (pkgs.stdenv.system != "x86_64-darwin") sumneko-lua-language-server;
  # }}}
}
