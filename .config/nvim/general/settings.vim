if &compatible
  set nocompatible
endif

syntax on
filetype plugin indent on

set visualbell
set shortmess=Ia
set shell=$SHELL
set termencoding=utf-8
set encoding=utf-8
set autoindent
set smartindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set shiftround
set expandtab
set smarttab
set history=1000
set backspace=indent,eol,start
set nostartofline
set showcmd
set isfname-==
set path+=**
set ttyfast
set t_Co=256
set lazyredraw
set synmaxcol=300
set noerrorbells
set autoread
set showmatch
set matchtime=2
set display=lastline
set number
set relativenumber
set cursorline
set nrformats-=octal
set nojoinspaces
set mouse=a
set mousehide
set hlsearch
set incsearch
set wrap
set linebreak
set ignorecase
set smartcase
set title
set updatetime=2000
set scrolloff=5
set sidescrolloff=5
set gdefault
set virtualedit=block
set foldlevelstart=99
set foldmethod=indent
set foldnestmax=5
set exrc
set secure
set tags^=.tags
set modeline
set modelines=2
set diffopt+=vertical
set splitright
set splitbelow
set hidden
set rnu
set noshowmode
set signcolumn=yes
set pastetoggle=<F2>

" On quit reset title
let &titleold=getcwd()

" Default text width to 80
if &textwidth == 0
  set textwidth=80
endif

" Don't have a text width for readonly files
if &readonly
  set textwidth=0
endif

" Completion options
set complete=.,w,b,u,t,i,kspell
set completeopt=menu
set wildmenu                                           " Better completion in the CLI
set wildmode=longest:full,full                         " Completion settings

" Ignore these folders for completions
set wildignore+=.hg,.git,.svn                          " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg         " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.pyc " compiled object files
set wildignore+=tags,.tags

" Set mapping and key timeouts
set timeout
set timeoutlen=1000
set ttimeoutlen=100

" Setting to indent wrapped lines
if exists('+breakindent')
  set breakindent
  set breakindentopt=shift:2
endif

" Use system clipboard
if has('clipboard')
  set clipboard=unnamed
  if has('unnamedplus')
    set clipboard+=unnamedplus
  endif
endif

if &diff
  set modifiable
  set noreadonly
endif

match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

if has('gui')
  set guifont="JetBrainsMono Nerd Font 11"
  set guioptions-=T
  set guioptions-=e
  set guioptions-=L
  set guioptions-=l
  set guioptions-=R
  set guioptions-=r
  set guioptions-=b
endif

set shell=bash

let mapleader=" "

augroup vimrc
  autocmd!
  autocmd GuiEnter * set columns=120 lines=70 number
augroup END

" ObjC curly brace error fix
let g:c_no_curly_error = 1

" https://github.com/thoughtbot/dotfiles/pull/471
let g:is_posix = 1

" Xcode
let g:xcode_runner_command = 'VtrSendCommandToRunner! {cmd}'
let g:xcode_xcpretty_testing_flags = '--test'
let g:xcode_default_simulator = 'iPhone 8'

" Tmux
let g:VtrGitCdUpOnOpen = 1

" Tern
let g:tern#command = ['tern']
let g:tern#arugments = ['--persistent']

" Jedi
let g:jedi#completions_enabled = 1
let g:jedi#popup_select_first = 0

" Supertab
let g:SuperTabDefaultCompletionType = "<c-x><c-o>"

let g:jsx_ext_required = 1
