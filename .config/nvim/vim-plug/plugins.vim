if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')

  " Miscellaneous
  Plug 'ervandew/supertab'
  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'godlygeek/tabular'
  Plug 'rbong/vim-flog'
  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-bundler'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-cucumber'
  Plug 'tpope/vim-eunuch'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-git'
  Plug 'tpope/vim-haml'
  Plug 'tpope/vim-projectionist'
  Plug 'tpope/vim-ragtag'
  Plug 'tpope/vim-rails'
  Plug 'tpope/vim-rake'
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-rsi'
  Plug 'tpope/vim-sensible'
  Plug 'tpope/vim-sleuth'
  Plug 'tpope/vim-speeddating'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-unimpaired'
  Plug 'tpope/vim-vinegar'
  Plug 'tpope/vim-vividchalk'
  Plug 'tpope/vim-dispatch'
  Plug 'tpope/vim-dadbod'
  Plug 'scrooloose/nerdtree'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'christoomey/vim-tmux-runner'
  Plug 'sbdchd/neoformat'
  Plug 'airblade/vim-gitgutter'
  Plug 'diepm/vim-rest-console'
  Plug '/usr/local/opt/fzf'
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'wellle/tmux-complete.vim'
  Plug 'davidhalter/jedi-vim'
  Plug 'itchyny/lightline.vim'
  Plug 'vim-test/vim-test'
  Plug 'junegunn/gv.vim'
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
  Plug 'plasticboy/vim-markdown'
  Plug 'amix/vim-zenroom2'
  Plug 'liuchengxu/vim-which-key'

  " Language
  Plug 'leafgarland/typescript-vim'
  Plug 'keith/swift.vim'

  " CoC
  Plug 'neoclide/coc.nvim', { 'branch': 'release' }

  " Themes
  Plug 'trevordmiller/nova-vim'
  Plug 'altercation/vim-colors-solarized'
  Plug 'brandonshega/material-vim'
  Plug 'junegunn/seoul256.vim'
  Plug 'dracula/vim', { 'as': 'dracula'}
  Plug 'connorholyday/vim-snazzy'

call plug#end()

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
