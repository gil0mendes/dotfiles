""" Plugins
" Specify the plugins directory
call plug#begin('~/.local/share/nvim/plugged')

" Theme
Plug 'ntk148v/vim-horizon'

" Install airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Better Visual Guide
Plug 'Yggdroot/indentLine'

" Multiple cursors
Plug 'terryma/vim-multiple-cursors'

" Add extra language support
Plug 'sheerun/vim-polyglot'

" Enable fuzzy search
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

" Enable autocompletion
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

" Completion sources
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'

"" CSS
Plug 'ncm2/ncm2-cssomni'

"" JavaScript
Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}

" Enable Lint
Plug 'w0rp/ale'

" Allow close or delete pairs
Plug 'jiangmiao/auto-pairs'

" Enable javascript autoimport
Plug 'ludovicchabant/vim-gutentags'
Plug 'kristijanhusak/vim-js-file-import', {'do': 'npm install'}

" Initalize  plugin system
call plug#end()

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" Autocomplete
set completeopt=noinsert,menuone,noselect

" Automatically fix lint errors on save
let g:ale_fixers = {}
let g:ale_fixers.javascript = ['eslint']
let g:ale_fix_on_save = 1

" Config Pyhton path
let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'

filetype plugin indent on

" Enable syntax
syntax on
syntax enable

" Set default encoding to UTF-8
set encoding=utf-8

" Define theme
colorscheme horizon 
set background=dark

" True Color Support if it's avaiable in terminal
if has("termguicolors")
    set termguicolors
endif
if has("gui_running")
  set guicursor=n-v-c-sm:block,i-ci-ve:block,r-cr-o:blocks
endif

" Enable relative line numbering
set number
set relativenumber

" Turn off backup
set nobackup
set noswapfile
set nowritebackup

" Tab and Indent configuration
set expandtab
set tabstop=2
set shiftwidth=2

" Maps system keyboard to Vim's past buffer
set clipboard=unnamedplus

" Allow to edit multiple files at the same time
set hidden

" Enable mouse
set mouse=a

" Enable realtime replace preview
set inccommand=split

" Airline
let g:airline_left_sep  = ''
let g:airline_right_sep = ''
let g:airline#extensions#ale#enabled = 1
let airline#extensions#ale#error_symbol = 'E:'
let airline#extensions#ale#warning_symbol = 'W:'
let g:airline_theme='base16'

""" Keymaps
let mapleader="\<space>"
nnoremap <leader>ec :vsplit ~/.config/nvim/init.vim<cr>
nnoremap <leader>rc :source ~/.config/nvim/init.vim<cr>

" Fuzzy search
nnoremap <c-p> :Files<cr>

" Search in all dirs
nnoremap <c-f> :Ag<space>

" Save during insertion
inoremap <c-s> <esc>:w<cr>i
