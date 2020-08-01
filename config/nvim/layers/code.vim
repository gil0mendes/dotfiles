" Automatically fix lint errors on save
let g:ale_fixers = {}
let g:ale_fixers.javascript = ['eslint']
let g:ale_fixers.html = []
let g:ale_fix_on_save = 1

" Coc

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
