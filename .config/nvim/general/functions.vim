" https://www.reddit.com/r/vim/comments/4jy1mh/slightly_more_subltle_n_and_n_behavior/
" Keep search matches in the middle of the window unless the next match is in
" the same viewport
function! NextAndCenter(cmd)
  let view = winsaveview()
  try
    execute "normal! " . a:cmd
  catch /^Vim\%((\a\+)\)\=:E486/
    " Fake a 'rethrow' of an exception without causing a 3 line error message
    echohl ErrorMsg
    echo "E486: Pattern not found: " . @/
    echohl None
  endtry

  if view.topline != winsaveview().topline
    normal! zzzv
  endif
endfunction

function! CloseLists()
  lclose
  cclose
  pclose
  silent! TagbarClose
endfunction

function! ClearWhitespace()
  let l:backup = @/
  let l:line = line('.')
  let l:column = col('.')
  silent! %s/\s\+$//e
  let @/=l:backup
  call cursor(l:line, l:column)
endfunction

function! ClearWhitespaceIfExpected()
  if &ft =~? 'markdown'
    return
  endif

  call ClearWhitespace()
endfunction

" Position resume
function! PositionRecall()
  if &ft =~? 'gitcommit\|gitrebase'
    return
  endif

  if line("'\"") > 0 && line("'\"") <= line('$')
    execute "normal g`\"zz"
  endif
endfunction

" Toggle relative numbering, and set to absolute on loss of focus or insert mode
function! ToggleNumbersOn()
    set nu!
    set rnu
endfunction
function! ToggleRelativeOn()
    set rnu!
    set nu
endfunction

function! s:warp_denite(cmd) abort
  exe a:cmd
  doautocmd WinEnter
endfunction
