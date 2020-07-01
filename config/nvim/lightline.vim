function! LightlineRelativepath()
  let relativepath = expand('%:f') !=# '' ? expand('%:f') : '[No Name]'
  let modified = &modified ? ' +' : ''
  return relativepath . modified
endfunction

function! LightlineFileformat()
  return winwidth(0) > 95 ? &fileformat : ''
endfunction

function! LightlineObsession()
  return '%{ObsessionStatus("●", "", "■")}'
endfunction

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

function! LightlineReadonly()
  return &readonly ? '' : ''
endfunction

let g:lightline = {
\   'active': {
\       'left': [
\                 ['mode', 'paste'],
\                 [ 'gitbranch' ],
\                 ['relativepath', 'cocstatus', 'currentfunction', 'readonly']
\               ],
\       'right': [
\                 [ 'lineinfo' ],
\                 [ 'percent' ],
\                 [ 'filetype', 'fileformat' ],
\                 [ 'gitblame', 'linter_errors', 'linter_warnings' ]
\               ]
\   },
\   'component_expand': {
\     'obsession': 'LightlineObsession',
\   },
\   'component_type': {
\       'readonly': 'error',
\       'linter_warnings': 'warning',
\       'linter_errors': 'error'
\   },
\   'component_function': {
\       'relativepath': 'LightlineRelativepath',
\       'fileformat': 'LightlineFileformat',
\       'readonly': 'LightlineReadonly',
\       'cocstatus': 'coc#status',
\       'currentfunction': 'CocCurrentFunction',
\   },
\   'tabline': {
\       'left': [ [ 'tabs' ] ],
\       'right': [ [ 'close' ] ]
\   },
\   'tab': {
\       'active': [ 'filename', 'modified' ],
\       'inactive': [ 'filename', 'modified' ],
\   },
\   'separator': { 'left': '', 'right': '' },
\   'subseparator': { 'left': '', 'right': '' }
\ }

