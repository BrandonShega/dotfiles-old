" ----- Configuration ----- "
set visualbell
set shortmess=Ia               " Disable the startup message - I, Avoid pressing enter after saves - a
set shell=$SHELL               " Set the default shell
set termencoding=utf-8         " Set the default encodings just in case $LANG isn't set
set encoding=utf-8             " Set the default encodings just in case $LANG isn't set
set autoindent                 " Indent the next line matching the previous line
set smartindent                " Smart auto-indent when creating a new line
set tabstop=2                  " Number of spaces each tab counts for
set shiftwidth=2               " The space << and >> moves the lines
set softtabstop=2              " Number of spaces for some tab operations
set shiftround                 " Round << and >> to multiples of shiftwidth
set expandtab                  " Insert spaces instead of actual tabs
set smarttab                   " Delete entire shiftwidth of tabs when they're inserted
set history=1000               " The number of history items to remember
set backspace=indent,eol,start " Backspace settings
set nostartofline              " Keep cursor in the same place after saves
set showcmd                    " Show command information on the right side of the command line
set isfname-==                 " Remove characters from filenames for gf
set path+=**
set ttyfast                    " Set that we have a fast terminal
set t_Co=256                   " Explicitly tell Vim that the terminal supports 256 colors
set lazyredraw                 " Don't redraw vim in all situations
set synmaxcol=300              " The max number of columns to try and highlight
set noerrorbells               " Don't make noise
set autoread                   " Watch for file changes and auto update
set showmatch                  " Set show matching parenthesis
set matchtime=2                " The amount of time matches flash
set display=lastline           " Display super long wrapped lines
set number                     " Shows line numbers
set relativenumber             " Relative line numbers
set cursorline                 " Highlight the line the cursor is on
set nrformats-=octal           " Never use octal notation
set nojoinspaces               " Don't add 2 spaces when using J
set mouse=a                    " Enable using the mouse if terminal emulator
set mousehide                  " Hide the mouse on typing
set hlsearch                   " Highlight search terms
set incsearch                  " Show searches as you type
set wrap                       " Softwrap text
set linebreak                  " Don't wrap in the middle of words
set ignorecase                 " Ignore case when searching
set smartcase                  " Ignore case if search is lowercase, otherwise case-sensitive
set title                      " Change the terminal's title
set updatetime=2000            " Set the time before plugins assume you're not typing
set scrolloff=5                " Lines the cursor is to the edge before scrolling
set sidescrolloff=5            " Same as scrolloff but horizontal
set gdefault                   " Adds g at the end of substitutions by default
set virtualedit=block          " Allow the cursor to move off the side in visual block
set foldlevelstart=99          " Set the default level of open folds
set foldmethod=indent          " Decide where to fold based
set foldnestmax=5              " Set deepest fold to x levels
set exrc                       " Source local .vimrc files
set secure                     " Don't load autocmds from local .vimrc files
set tags^=.tags                " Add local .tags file
set modeline                   " Check for file specific vim settings in the last 3 lines of the file
set modelines=2
set diffopt+=vertical          " Always use vertical diffs
set splitright
set splitbelow
set hidden
set rnu
set noshowmode
set signcolumn=yes

" F2 before pasting to preserve indentation
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

set background=dark
colorscheme seoul256
