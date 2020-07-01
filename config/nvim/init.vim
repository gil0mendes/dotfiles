" Settings {{{
" Note: Some settings provided by vim-sensible

" No backup files
set nobackup

" No write bakups
set nowritebackup

" No swap file
set noswapfile

" Show incomplete commands
set showcmd

" A buffer is marked as 'hidden' if it has unsaved cahnges, and it is not
" currently loaded in a window.
" If you try and quit Vim while there are hidden buffers, you will raise an
" error:
" E162: No write since last change for buffer "a.txt"
set hidden

" Turn word wrap off
set nowrap

" Don't break long lines in insert mode
set formatoptions=l

" Convert tabs to spaces, all file types
set expandtab
" Set tab size in space (this is for manual indenting)
set tabstop=2
set softtabstop=2 " edit as if the tabs are 2 characters wide
set shiftwidth=2

" code folding syntax
set foldmethod=syntax " fold based on indent
set foldlevelstart=99
set foldnestmax=10 " deepest fold is 10 levels
set nofoldenable " don't fold by default
set foldlevel=1

" The number of line numbers AND use relative number
set number
set relativenumber

set diffopt+=vertical,iwhite,internal,algorithm:patience,hiddenoff

" Highlight tabs and trailing whitespace
set list
set list listchars=tab:»·,trail:·,extends:>,precedes:<,nbsp:+

"listchars Use system clipboard
" http://stackoverflow.com/questions/8134647/copy-and-paste-in-vim-via-keyboard-between-different-mac-terminals
set clipboard+=unnamed

" don't give |ins-completion-menu| messages.
set shortmess+=c

" define the shell to use
set shell=$SHELL

" How many tenths of a second to blink
set mat=2

" Better splits (new windows appear below and to the right)
set splitbelow
set splitright

" Highlight the current line and column
set cursorline

" Ensure Vim doesn't beep at you every time you make a mistype
set visualbell

" redraw only when we need to (i.e. don't redraw when executing a macro)
set lazyredraw

" highlight a matching [{()}] when cursor is placed on start/end character
set showmatch

" Display the mode you're in.
" set showmode
set noshowmode " Or not...

" Complete files like a shell.
set wildmode=list:longest

" Show 2 lines of context around the cursor.
set scrolloff=2

" Set the terminal's title
set title

" Vertical line at 120 characters
set textwidth=120
set colorcolumn=+1

" Start diff mode with vertical splits
set diffopt=vertical

" always show signcolumns
set signcolumn=auto

" Set built-in file system explorer to use layout similar to the NERDTree plugin
let g:netrw_liststyle=3

" Enable built-in matchit plugin
runtime macros/matchit.vim

set grepprg=rg

let g:grep_cmd_opts = '--line-numbers --noheading --ignore-dir=log --ignore-dir=tmp'

set inccommand=nosplit

set updatetime=300

" Keep focus split wide, others narrow.
set winwidth=90
set winminwidth=5

" Keep focus split at max height, others minimal.
set winheight=1
set winminheight=1

" Searching
set ignorecase            " Ignore case in search if terms(s) are lowercase
set smartcase             " Search with case sensitive if term(s) are upper or mixed case
set hlsearch              " Highlight search results
set magic                 " set magic on for regex
set nolazyredraw          " Don't redraw while executing macros

" Enable realtime replace preview
set inccommand=split

" error bells
set noerrorbells
set visualbell
set t_vb=
set tm=500


" True Color Support if it's avaiable in terminal
if has("termguicolors")
  if (!(has("nvim")))
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif

  set termguicolors
endif

if &term =~ '256color'
  " disable background color erase
  set t_ut=
endif

" Enable mouse
if has('mouse')
  set mouse=a
endif

set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors

if has("gui_running")
  set guicursor=n-v-c-sm:block,i-ci-ve:block,r-cr-o:blocks
endif

" switch cursor to line when in insert mode, and block when not
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
\,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
\,sm:block-blinkwait175-blinkoff150-blinkon175

" highlight conflicts
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
" }}}

" Commands {{{
" specify syntax highlighting for specify files
augroup file_types
  autocmd!
  autocmd Bufread,BufNewFile *Brewfile,pryrc set filetype=ruby
  autocmd Bufread,BufNewFile *prettierrc,*stylelintrc,*babelrc set filetype=json
  autocmd Bufread,BufNewFile aliases,functions,prompt,tmux,opts set filetype=zsh
  autocmd Bufread,BufNewFile gitconfig set filetype=gitconfig
augroup END

" Remove trailing whitespace on save for specified file types.
augroup clear_whitespace
  autocmd!
  au BufWritePre *.rb,*.yml,*.erb,*.haml,*.css,*.scss,*.js,*.coffee,*.vue :%s/\s\+$//e
augroup END

" Fold settings
augroup fold_settings
  autocmd!
  autocmd FileType json setlocal foldmethod=syntax
  autocmd FileType json normal zR
augroup END

" Close vim if only nerdtree window is left
augroup nerdtree_settings
  autocmd!
  autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
augroup END

" automatically rebalance windows on vim resize
augroup window_resize
  autocmd!
  autocmd VimResized * :wincmd =
augroup END

" https://github.com/reedes/vim-textobj-quote#configuration
augroup textobj_quote
  autocmd!
  autocmd FileType markdown call textobj#quote#init()
  autocmd FileType textile call textobj#quote#init()
  autocmd FileType text call textobj#quote#init({'educate': 0})
augroup END

" }}}


""" Plugins {{{

" Specify the plugins directory
call plug#begin('~/.local/share/nvim/plugged')

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

" Code Completion
Plug 'neoclide/coc.nvim',
      \ {'branch': 'release', 'do': 'yarn install --frozen-lockfile'}                         " Intellisense engine for vim8 & neovim   | https://github.com/neoclide/coc.nvim

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

" Tim Pope
Plug 'tpope/vim-surround'               " Quoting/parenthesizing made simple                    | https://github.com/tpope/vim-surround
Plug 'tpope/vim-sensible'               " Defaults everyone can agree on                        | https://github.com/tpope/vim-sensible

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

" Autocomplete
set completeopt=noinsert,menuone,noselect

" Automatically fix lint errors on save
let g:ale_fixers = {}
let g:ale_fixers.javascript = ['eslint']
let g:ale_fix_on_save = 1

" Config Pyhton path
let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'

" Plugin-specific Mappings & Settings
let g:far#source = "agnvim"
let g:far#file_mask_favorites = ['%', '**/*.html', '**/*.js', '**/*.css', '**/*.ex', '**/*.ts', '**/*.tsx']

" NERDTree
let NERDTreeShowHidden=1
let NERDTreeIgnore=['\.png$', '\.jpg$', '\.gif$', '\.mp3$', '\.ogg$', '\.mp4$',
      \ '\.avi$','.webm$','.mkv$','\.pdf$', '\.zip$', '\.tar.gz$',
      \ '\.rar$', '\.git$']
let NERDTreeDirArrowExpandable = "\u00a0" " make arrows invisible
let NERDTreeDirArrowCollapsible = "\u00a0" " make arrows invisible
let NERDTreeNodeDelimiter = "\u263a" " smiley face
let NERDTreeMinimalUI=1

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

let g:NERDTreeIndicatorMapCustom = {
\ "Modified"  : "✹",
\ "Staged"    : "✚",
\ "Untracked" : "",
\ "Renamed"   : "➜",
\ "Unmerged"  : "═",
\ "Deleted"   : "✖",
\ "Dirty"     : "~",
\ "Clean"     : "✔︎",
\ 'Ignored'   : '☒',
\ "Unknown"   : ""
\ }

" Vim DevIcons
let g:WebDevIconsOS = 'Darwin'
let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:DevIconsEnableFoldersOpenClose = 1
let g:DevIconsEnableFolderExtensionPatternMatching = 1

" IndentLine
let g:indentLine_char = '|'

" TComment
map <leader>/ :TComment<CR>

" FZF
let g:fzf_layout = { 'down': '~25%' }

if isdirectory(".git")
  " if in a git project, use :GFiles
  nmap <silent> <c-p> :GitFiles --cached --others --exclude-standard<cr>
else
  " otherwise, use :FZF
  nmap <silent> <c-p> :FZF<cr>
endif


command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   fzf#vim#with_preview(), <bang>0)

" Search in all dirs
" nnoremap <c-f> :Ag<space>

" Coc
" https://github.com/neoclide/coc.nvim

" Global extension names to install when they aren't installed
let g:coc_global_extensions = [
\ 'coc-css',
\ 'coc-html',
\ 'coc-json',
\ 'coc-tsserver',
\ 'coc-vetur',
\ 'coc-yaml',
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

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

autocmd CursorHold * silent call CocActionAsync('highlight')

"remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gh <Plug>(coc-doHover)

" rename
nmap <F2> <Plug>(coc-rename)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>


" Load Startify configurations
exec 'source' stdpath('config') . '/startify.vim'


" Git Fugituve {{{
  " maybe the best git wrapper
  Plug 'tpope/vim-fugitive'

  nmap <silent> <leader>gs :Gstatus<cr>
  nmap <leader>ge :Gedit<cr>
  nmap <silent><leader>gr :Gread<cr>
  nmap <silent><leader>gb :Gblame<cr>

  Plug 'junegunn/gv.vim'
" }}}


" Manager buffers
" Use the right side of the screen
let g:buffergator_viewport_split_policy = 'R'

" View the entire list of buffers open
nmap <leader>, :BuffergatorOpen<cr>
nmap <leader>bl :BuffergatorOpen<cr>

" close buffer
nmap <leader>bc :bp <BAR> bd #<cr>

" close buffers but keep splits
nmap <leader>bd :Bdelete<cr>


" Mappings {{{
let mapleader = "\<Space>"

" bind to simply config edit and reload
nnoremap <leader>ce :vsplit ~/.config/nvim/init.vim<cr>
nnoremap <leader>cr :exec 'source' stdpath('config') . '/init.vim'<cr>

"allow to save using crtl-s
nmap <C-s> :w<cr>
imap <C-s> <esc>:w<cr>i

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

" allow moving blocks of likes with Alt+j/k
nnoremap ¯ :m .+1<CR>==
nnoremap „ :m .-2<CR>==
inoremap ¯ <Esc>:m .+1<CR>==gi
inoremap „ <Esc>:m .-2<CR>==gi
vnoremap ¯ :m '>+1<CR>gv=gv
vnoremap „ :m '<-2<CR>gv=gv

" Clear highlight
map <leader>h :nohl<CR>

" force me to use h, j, k, and l keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Save during insertion
inoremap <c-s> <esc>:w<cr>e

" Toggle fix cursor on center if possible
nnoremap <leader>ll :let &scrolloff=999-&scrolloff<CR>

" Define theme
colorscheme nord
set background=dark

" load lightline configurations
exe 'source' stdpath('config') . '/lightline.vim'

" load colorscheme specific config
exe 'source' stdpath('config') . '/colorschemes/nord.vim'
