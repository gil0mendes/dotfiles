" Base vim settings
"
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

" Stop time-limiting leader key conbination
" set notimeout

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

" define leader key
let g:mapleader = "\<Space>"
" let g:maplocalleader = ','

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

" Autocomplete
set completeopt=noinsert,menuone,noselect

" Config Pyhton path
let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'

