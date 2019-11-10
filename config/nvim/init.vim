""" Plugins
" Specify the plugins directory
call plug#begin('~/.local/share/nvim/plugged')

" Theme
Plug 'ntk148v/vim-horizon'

" Install airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" tmux integration for vim
Plug 'benmills/vimux'

" Better Visual Guide
Plug 'Yggdroot/indentLine'

" Multiple cursors
Plug 'terryma/vim-multiple-cursors'

" Enable colors
Plug 'chrisbra/Colorizer'

" Add extra language support
Plug 'sheerun/vim-polyglot'

" Enable autocompletion
"Plug 'ncm2/ncm2'
"Plug 'roxma/nvim-yarp'

" Completion sources
"Plug 'ncm2/ncm2-bufword'
"Plug 'ncm2/ncm2-path'

"" CSS
"Plug 'ncm2/ncm2-cssomni'

"" JavaScript
"Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}

" Enable Lint
Plug 'w0rp/ale'

" Allow close or delete pairs
Plug 'jiangmiao/auto-pairs'

" Enable javascript autoimport
Plug 'ludovicchabant/vim-gutentags'
Plug 'kristijanhusak/vim-js-file-import', {'do': 'npm install'}

" enable ncm2 for all buffers
" autocmd BufEnter * call ncm2#enable_for_buffer()

" Autocomplete
set completeopt=noinsert,menuone,noselect

" Automatically fix lint errors on save
let g:ale_fixers = {}
let g:ale_fixers.javascript = ['eslint']
let g:ale_fix_on_save = 1

" Config Pyhton path
let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'

""""""""""""""""""""""""""""""""""
" general
""""""""""""""""""""""""""""""""""

" True Color Support if it's avaiable in terminal
if has("termguicolors")
    set termguicolors
endif
if has("gui_running")
  set guicursor=n-v-c-sm:block,i-ci-ve:block,r-cr-o:blocks
endif

set autoread " defect when a file is changed

" Turn off backup
set nobackup
set noswapfile
set nowritebackup

" set backspace=indent,eol,start " make backspace behave in a sane manner
set clipboard=unnamedplus " maps system keyboard to Vim's past buffer

" Allow to edit multiple files at the same time
" set hidden

" Enable mouse
if has('mouse')
  set mouse=a
endif

" Searching
set ignorecase " ignore insensative searching
set smartcase " case-sensitive if expresson contains a capital letter
set hlsearch " highlight search results
set incsearch " set incremental search, like modern browsers
set nolazyredraw " don't redraw while executing macros

set magic " set magic on for regex

" Enable realtime replace preview
set inccommand=split

" error bells
set noerrorbells
set visualbell
set t_vb=
set tm=500

""""""""""""""""""""""""""
" apparence
""""""""""""""""""""""""""

set number " show line numbers
set relativenumber " enable relative number
set autoindent " enable out indent of a new line
set ttyfast " fast redraw
set diffopt+=vertical,iwhite,internal,algorithm:patience,hiddenoff
set laststatus=2 " show the status line all the time
set hidden " current buffer can be put into background
set showcmd " show incomplete commands
set noshowmode " don't show which mode disabled for PowerLine
set shell=$SHELL
set cmdheight=1 " command bar height
set title " set terminal title
set showmatch " show matching braces
set mat=2 " how many tenths of a second to blink
set updatetime=300
set signcolumn=yes
set shortmess+=c

" Tab and Indent configuration
set smarttab " tab respects 'tabstop', 'shiftwidth', and 'softtabstop'
set expandtab
set tabstop=2
set softtabstop=2 " edit as if the tabs are 2 characters wide
set shiftwidth=2

" code folding syntax
set foldmethod=syntax " fold based on indent
set foldlevelstart=99
set foldnestmax=10 " deepest fold is 10 levels
set nofoldenable " don't fold by default
set foldlevel=1

" toggle invisible characters
set list
set listchars=tab:→\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
set showbreak=↪

set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors
" switch cursor to line when in insert mode, and block when not
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
\,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
\,sm:block-blinkwait175-blinkoff150-blinkon175

if &term =~ '256color'
  " disable background color erase
  set t_ut=
endif

" enable 24 bit color support if supported
if (has("termguicolors"))
  if (!(has("nvim")))
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif
  set termguicolors
endif

" highlight conflicts
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Airline
let g:airline_left_sep  = ''
let g:airline_right_sep = ''
let g:airline#extensions#ale#enabled = 1
let airline#extensions#ale#error_symbol = 'E:'
let airline#extensions#ale#warning_symbol = 'W:'
let g:airline_theme='base16'

""""""""""""""""""""""""
" Keymaps
""""""""""""""""""""""""

let mapleader="\<space>"

" bind to simply config edit and reload
nnoremap <leader>ec :vsplit ~/.config/nvim/init.vim<cr>
nnoremap <leader>rc :source ~/.config/nvim/init.vim<cr>

" keep visual selection when indenting/outdenting
vmap < <gv
vmap > >gv

" switch between current and last buffer
nmap <leader>. <c-^>

" enable . command in visual mode
vnoremap . :normal .<cr>

" allow to move win
map <silent> <C-h> <Plug>WinMoveLeft
map <silent> <C-j> <Plug>WinMoveDown
map <silent> <C-k> <Plug>WinMoveUp
map <silent> <C-l> <Plug>WinMoveRight

nmap <leader>z <Plug>Zoom

" scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" moving up and down work as you would expect
nnoremap <silent> j gj
nnoremap <silent> k gk
nnoremap <silent> ^ g^
nnoremap <silent> $ g$

" close buffers but keep splits
Plug 'moll/vim-bbye'
nmap <leader>b :Bdelete<cr>

" NERDTree {{{
  Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }
  Plug 'Xuyuanp/nerdtree-git-plugin'
  Plug 'ryanoasis/vim-devicons'
  Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

  let g:WebDevIconsOS = 'Darwin'
  let g:WebDevIconsUnicodeDecorateFolderNodes = 1
  let g:DevIconsEnableFoldersOpenClose = 1
  let g:DevIconsEnableFolderExtensionPatternMatching = 1
  let NERDTreeDirArrowExpandable = "\u00a0" " make arrows invisible
  let NERDTreeDirArrowCollapsible = "\u00a0" " make arrows invisible
  let NERDTreeNodeDelimiter = "\u263a" " smiley face

  augroup nerdtree
    autocmd!
    autocmd FileType nerdtree setlocal nolist " turn off whitespace characters
    autocmd FileType nerdtree setlocal nocursorline " turn off line highlighting for performance
  augroup END

  " Toggle NERDTree
  function! ToggleNerdTree()
    if @% != "" && @% !~ "Startify" && (!exists("g:NERDTree") || (g:NERDTree.ExistsForTab() && !g:NERDTree.IsOpen()))
      :NERDTreeFind
    else
      :NERDTreeToggle
    endif
  endfunction

  " toggle nerd tree
  nmap <silent> <leader>k :call ToggleNerdTree()<cr>

  " find the current file in nerdtree without needing to reload the drawer
  nmap <silent> <leader>y :NERDTreeFind<cr>

  let NERDTreeShowHidden=1

  let g:NERDTreeIndicatorMapCustom = {
  \ "Modified"  : "✹",
  \ "Staged"    : "✚",
  \ "Untracked" : "✭",
  \ "Renamed"   : "➜",
  \ "Unmerged"  : "═",
  \ "Deleted"   : "✖",
  \ "Dirty"     : "✗",
  \ "Clean"     : "✔︎",
  \ 'Ignored'   : '☒',
  \ "Unknown"   : "?"
  \ }
" }}}

" FZF {{{
  " Enable fuzzy search
  Plug '/usr/local/opt/fzf'
  Plug 'junegunn/fzf.vim'

  let g:fzf_layout = { 'down': '~25%' }

  if isdirectory(".git")
    " if in a git project, use :GFiles
    nmap <silent> <c-p> :GitFiles --cached --others --exclude-standard<cr>
  else
    " otherwise, use :FZF
    nmap <silent> <c-p> :FZF<cr>
  endif

  " Search in all dirs
  nnoremap <c-f> :Ag<space>
" }}}

" Git Fugituve {{{
  " maybe the best git wrapper
  Plug 'tpope/vim-fugitive'

  nmap <silent> <leader>gs :Gstatus<cr>
  nmap <leader>ge :Gedit<cr>
  nmap <silent><leader>gr :Gread<cr>
  nmap <silent><leader>gb :Gblame<cr>

  Plug 'junegunn/gv.vim'
" }}}

" coc {{{
  Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}

  let g:coc_global_extensions = [
  \ 'coc-css',
  \ 'coc-json',
  \ 'coc-tsserver',
  \ 'coc-git',
  \ 'coc-eslint',
  \ 'coc-tslint-plugin',
  \ 'coc-pairs',
  \ 'coc-sh',
  \ 'coc-vimlsp',
  \ 'coc-emmet',
  \ 'coc-prettier',
  \ 'coc-ultisnips'
  \ ]

  autocmd CursorHold * silent call CocActionAsync('highlight')
 
  "remap keys for gotos
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  nmap <silent> gh <Plug>(coc-doHover)

  " rename
  nmap <silent> <leader>rn <Plug>(coc-rename)

"" }}}

" Save during insertion
inoremap <c-s> <esc>:w<cr>

" Toggle fix cursor on center if possible
nnoremap <leader>ll :let &scrolloff=999-&scrolloff<CR>

call plug#end()

filetype plugin indent on

" Enable syntax
syntax on
syntax enable

" Set default encoding to UTF-8
set encoding=utf-8

" Define theme
colorscheme horizon 
set background=dark

" make the highlighting of tabs and other non-text less annoying
" highlight SpecialKey ctermfg=19 guifg=#333333
" highlight NonText ctermfg=19 guifg=#333333
