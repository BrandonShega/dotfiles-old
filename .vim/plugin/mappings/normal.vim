nnoremap Q :quit<CR>
nnoremap W :write<CR>
noremap j gj
noremap k gk
nnoremap <C-u> 10<C-u>
nnoremap ; :
nnoremap <silent> n :call NextAndCenter('n')<CR>
nnoremap <silent> N :call NextAndCenter('N')<CR>
nnoremap Y y$
noremap gk k
noremap gj j
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
nnoremap <silent> <bs> :TmuxNavigateLeft<cr>
nnoremap <CR> o<Esc>
noremap <F1> <Nop>
nnoremap <leader>pt :!prettier %<CR>
nnoremap <silent> <leader> <tab> <C-^>

" Running as diff
if &diff
  if tabpagenr('$') == 1
    nnoremap ZZ :wqall<CR>
  endif
else
  " Jump to next/previous merge conflict marker
  nnoremap <silent> ]c /\v^(\<\|\=\|\>){7}([^=].+)?$<CR>
  nnoremap <silent> [c ?\v^(\<\|\=\|\>){7}([^=].+)\?$<CR>
endif
