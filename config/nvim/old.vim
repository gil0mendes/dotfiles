

" Plugin-specific Mappings & Settings
let g:far#source = "agnvim"
let g:far#file_mask_favorites = ['%', '**/*.html', '**/*.js', '**/*.css', '**/*.ex', '**/*.ts', '**/*.tsx']

" IndentLine
let g:indentLine_char = '|'

" TComment
" TODO: move to commentry
" map <leader>/ :TComment<CR>

" Search in all dirs
" nnoremap <c-f> :Ag<space>

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

" enable . command in visual mode
vnoremap . :normal .<cr>

nmap <leader>z <Plug>Zoom

" Clear highlight
map <leader>h :nohl<CR>

