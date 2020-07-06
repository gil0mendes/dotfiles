" Configure Which Key
"
" TODO: on the future would be nice to have a global function to automatically map the keys inside which layers

" Map leader to which key

" Configure Which key to be used with the space key
nnoremap <silent> <leader> :silent <c-u> :silent WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :silent <c-u> :silent WhichKeyVisual '<Space>'<CR>

" nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

" timeout after key stroke
set timeoutlen=500


" Create map to add keys to
let g:which_key_map = {}

" Define a separator
let g:which_key_sep = '→'



" Not a fan of floating windows for this
let g:which_key_use_floating_win = 0


" Change the colors if you want
highlight default link WhichKey          Operator
highlight default link WhichKeySeperator DiffAdded
highlight default link WhichKeyGroup     Identifier
highlight default link WhichKeyDesc      Function


" Hide status line
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler


" Single mappings
let g:which_key_map['k'] = ['ToggleNerdTree', 'open sidebar']


" Register which key map
call which_key#register('<Space>', "g:which_key_map")
