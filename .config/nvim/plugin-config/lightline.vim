let g:lightline = {
  \ 'colorscheme': 'wombat',
  \ 'active' : {
  \   'left' : [ [ 'mode', 'paste' ],
  \              [ 'readonly', 'filename', 'modified' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'percent' ],
  \              [ 'cocstatus', 'gitbranch', 'fileformat', 'filetype' ] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'fugitive#head',
  \   'cocstatus': 'coc#status'
  \ }
  \ }
