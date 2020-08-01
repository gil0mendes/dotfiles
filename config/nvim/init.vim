"       ___  __         __   __   ___  __
" |\ | |__  /  \  |\/| /  \ |  \ |__  |__) |\ |
" | \| |___ \__/  |  | \__/ |__/ |___ |  \ | \|
"
" A modern VIM configuration to get all the good stuff from a moder editor without comming with an Eletron for free.
"
" Author: Gil Mendes <gil00mendes@gmail.com>
"

" Load global basic functions
exec 'source' stdpath('config') . '/functions.vim'

" load all plugins
call LoadConfigFile('plugins.vim')

" Load base settings
call LoadConfigFile("settings.vim")

" Load layers
call LoadConfigFile('layers/nerdtree.vim')
call LoadConfigFile('layers/search.vim')
call LoadConfigFile('layers/startify.vim')
call LoadConfigFile('layers/code.vim')

" Load theme for lightline
call LoadConfigFile('lightline.vim')

" Define theme
colorscheme nord
set background=dark
highlight Comment cterm=italic

" load colorscheme specific config
call LoadConfigFile('colorschemes/nord.vim')

" Mapping
call LoadConfigFile('which-key.vim')
call LoadConfigFile('mapping.vim')

