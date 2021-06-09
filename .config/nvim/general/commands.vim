command! -bang Q q<bang>
command! -bang -nargs=* -complete=file W w<bang> <args>
command! ClearWhitespace call ClearWhitespace()
