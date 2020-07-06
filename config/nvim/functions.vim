" Global functions
"
" This is the first think being loaded.

function LoadConfigFile(path)
  exec 'source' stdpath('config') . '/' . a:path
endfunction

