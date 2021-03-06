" Extra maps that doesn't fit on which-keys

" bind to simply config edit and reload
nnoremap <leader>ce :vsplit ~/.config/nvim/init.vim<cr>
nnoremap <leader>cr :exec 'source' stdpath('config') . '/init.vim'<cr>

" Allow to save using crtl-s
nmap <C-s> :w<cr>
imap <C-s> <esc>:w<cr>i
inoremap <c-s> <esc>:w<cr>e

" keep visual selection when indenting/outdenting
vmap < <gv
vmap > >gv

" switch between current and last buffer
nmap <leader>. <c-^>

" allow to move win
map <silent> <C-h> <Plug>WinMoveLeft
map <silent> <C-j> <Plug>WinMoveDown
map <silent> <C-k> <Plug>WinMoveUp
map <silent> <C-l> <Plug>WinMoveRight

" scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" moving up and down work as you would expect
nnoremap <silent> j gj
nnoremap <silent> k gk
nnoremap <silent> ^ g^
nnoremap <silent> $ g$

" allow moving blocks of likes with Alt+j/k
nnoremap <A-j> :m+<CR>==
nnoremap <A-k> :m-2<CR>==
inoremap <A-j> <Esc>:m+<CR>==gi
inoremap <A-k> <Esc>:m-2<CR>==gi
vnoremap <A-j> :m'>+<CR>gv=gv
vnoremap <A-k> :m-2<CR>gv=gv

" force me to use h, j, k, and l keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Toggle fix cursor on center if possible
nnoremap <leader>ll :let &scrolloff=999-&scrolloff<CR>

