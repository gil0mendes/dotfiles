" Colorscheme build in Lua with `lush.nvim`, modeled initially on Solarized, but intended to be usable with alternate 
" colors.
" Still early days, with lots of work needed
" See `../lua/g0m-theme.lua`
let g:colors_name = 'g0m'

" Load colorscheme
lua require'g0m.theme'.loadColorscheme()
