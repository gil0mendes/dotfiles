" List of all installed plugins

" Install autoinstall vim-plug
if empty(glob(stdpath('config') . '/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Specify the plugins directory
call plug#begin(stdpath('config') . '/autoload/plugged')

" Tim Pope plugins
Plug 'tpope/vim-eunuch'                 " Helpers for UNIX                                      | https://github.com/tpope/vim-eunuch
Plug 'tpope/vim-surround'               " Quoting/parenthesizing made simple                    | https://github.com/tpope/vim-surround
Plug 'tpope/vim-sensible'               " Defaults everyone can agree on                        | https://github.com/tpope/vim-sensible
Plug 'tpope/vim-repeat'                 " repeat.vim: enable repeating for all commands         | https://github.com/tpope/vim-repeat
Plug 'tpope/vim-commentary'             " Better Comments                                       | https://github.com/tpope/vim-commentary
Plug 'tpope/vim-sleuth'                 " Heuristically set buffer options                      | https://github.com/tpope/vim-sleuth

" General
Plug 'mhinz/vim-startify'               " The fancy start screen for Vim.                       | https://github.com/mhinz/vim-startify
Plug 'godlygeek/tabular'                " Vim script for text filtering and alignment           | https://github.com/godlygeek/tabular
Plug 'tomtom/tcomment_vim'              " An extensible & universal comment vim-plugin          | https://github.com/tomtom/tcomment_vim
Plug 'vim-scripts/BufOnly.vim'          " Delete all the buffers except current/named buffer    | https://github.com/vim-scripts/BufOnly.vim
Plug 'jeetsukumaran/vim-buffergator'    " Open/close/navigate vim's buffers                     | https://github.com/jeetsukumaran/vim-buffergator
Plug 'moll/vim-bbye'                    " Really close buffers                                  | https://github.com/moll/vim-bbye
Plug 'ntpeters/vim-better-whitespace'   " Better whitespace highlighting for                    | https://github.com/ntpeters/vim-better-whitespace
Plug 'machakann/vim-highlightedyank'    " Make the yanked region apparent!                      | https://github.com/machakann/vim-highlightedyank
Plug 'norcalli/nvim-colorizer.lua'      " The fastest Neovim colorizer.                         | https://github.com/norcalli/nvim-colorizer.lua
Plug 'liuchengxu/vim-which-key'         " Vim plugin that shows keybindings in popup            | https://github.com/liuchengxu/vim-which-key

" Code Completion
Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}                         " Intellisense engine for vim8 & neovim   | https://github.com/neoclide/coc.nvim

" Searching
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

" Navigation
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }      " A tree explorer plugin for vim                  | https://github.com/scrooloose/nerdtree
Plug 'Xuyuanp/nerdtree-git-plugin'                                            " A plugin of NERDTree showing git status         | https://github.com/Xuyuanp/nerdtree-git-plugin
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'                                " Extra syntax and highlight for nerdtree files   | https://github.com/tiagofumo/vim-nerdtree-syntax-highlight
Plug 'brooth/far.vim'                                                         " Far Vim Find And Replace Vim plugin             | https://github.com/brooth/far.vim
Plug 'easymotion/vim-easymotion'                                              " Vim motions on speed!                           | https://github.com/easymotion/vim-easymotion
Plug 'ryanoasis/vim-devicons'                                                 " Adds file type icons to Vim                     | https://github.com/ryanoasis/vim-devicons
Plug 'itchyny/lightline.vim'                                                  " A light and configurable statusline/tabline plugin for Vim | https://github.com/itchyny/lightline.vim

" Nord Theme
Plug 'arcticicestudio/nord-vim'

" Syntax Highlight
Plug 'hail2u/vim-css3-syntax'           " CSS3 syntax                                           | https://github.com/hail2u/vim-css3-syntax
Plug 'pangloss/vim-javascript',
      \ { 'for': ['javascript', 'vue']
      \}                                " Javascript indentation and syntax support in Vim.     | https://github.com/pangloss/vim-javascript
Plug 'maxmellon/vim-jsx-pretty'         " JSX and TSX syntax pretty highlighting for vim.       | https://github.com/MaxMEllon/vim-jsx-pretty
Plug 'posva/vim-vue'                    " Syntax Highlight for Vue.js components                | https://github.com/posva/vim-vue
Plug 'elzr/vim-json'                    " A better JSON for Vim                                 | https://github.com/elzr/vim-json
Plug 'cespare/vim-toml'                 " Vim syntax for TOML                                   | https://github.com/cespare/vim-toml
Plug 'leafgarland/typescript-vim'       " Typescript syntax files for Vim                       | https://github.com/leafgarland/typescript-vim
Plug 'Yggdroot/indentLine'              " Display indention levels with thin vertical lines     | https://github.com/Yggdroot/indentLine

" tmux integration for vim
Plug 'benmills/vimux'

" Multiple cursors
Plug 'terryma/vim-multiple-cursors'

" Enable colors
Plug 'chrisbra/Colorizer'

" Add extra language support
Plug 'sheerun/vim-polyglot'

" Enable Lint
Plug 'w0rp/ale'

" Allow close or delete pairs
Plug 'jiangmiao/auto-pairs'

" Enable javascript autoimport
Plug 'ludovicchabant/vim-gutentags'
Plug 'kristijanhusak/vim-js-file-import', {'do': 'npm install'}

call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
