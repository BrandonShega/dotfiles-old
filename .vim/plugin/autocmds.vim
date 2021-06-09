if has('autocmd')
  augroup ft_settings
    autocmd!

    " Comment string settings
    if empty(&commentstring) | setlocal commentstring=#\ %s | endif
    autocmd FileType c,cpp,go,objc,php setlocal commentstring=//\ %s

    " Treat .ipas as .zip files
    autocmd BufReadCmd *.ipa call zip#Browse(expand("<amatch>"))

    " Save files on some focus lost events, like switching splits
    autocmd BufLeave,FocusLost * silent! wall

    " Don't auto insert a comment when using O/o for a newline
    autocmd VimEnter,BufRead,FileType * set formatoptions-=o

    " Return to the same position you left the file in
    autocmd BufRead * call PositionRecall()

    " Clear whitespace on save
    autocmd BufWritePre * call ClearWhitespaceIfExpected()

    autocmd CursorHold <buffer> checktime

    autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
    autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
    autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
    autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
    autocmd FileType ruby compiler ruby
  augroup END

  augroup relative_numbers
    autocmd!

    autocmd FocusLost * call ToggleRelativeOn()
    autocmd FocusGained * call ToggleRelativeOn()
    autocmd InsertEnter * call ToggleRelativeOn()
    autocmd InsertLeave * call ToggleRelativeOn()
  augroup END

  augroup quickfix
    autocmd!

    "Use enter to create new lines w/o entering insert mode
    "Below is to fix issues with the ABOVE mappings in quickfix window
    autocmd CmdwinEnter * nnoremap <CR> <CR>
    autocmd BufReadPost quickfix nnoremap <CR> <CR>
  augroup END

  autocmd FileType java setlocal omnifunc=javacomplete#Complete

  autocmd BufEnter * :syntax sync fromstart
endif
