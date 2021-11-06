fpath=(
  $fpath
  ~/.rvm/scripts/zsh/Completion
  ~/.zsh/functions
  /usr/local/share/zsh/site-functions
)

# Autoload
autoload colors; colors
autoload -U compinit; compinit
autoload edit-command-line
zle -N edit-command-line

# Options
setopt appendhistory
setopt extendedglob
setopt histignoredups
setopt nonomatch
setopt prompt_subst
setopt interactivecomments
setopt autoparamslash
setopt autopushd
setopt correct
setopt correctall
setopt autocd

# History
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=10000
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY

# Functions
l.() {
  ls -ld "${1:-$PWD}"/.[^.]*
}

emacs.() {
  emacsclient -c -a= $*
}

# default apps
(( ${+PAGER}   )) || export PAGER='less'
(( ${+EDITOR}  )) || export EDITOR='emacs'
export PSQL_EDITOR='emacs'

# Aliases
alias reload='source ~/.zshrc; echo -e "\n\u2699  \e[33mZSH config reloaded\e[0m \u2699"'

# Import
zrcl="$HOME/.zshrc.local"
[[ ! -a $zrcl ]] || source $zrcl

# Misc
cdpath=(~ ~/src $DEV_DIR $SOURCE_DIR)
typeset -aU path

# Sources
source $HOME/.zsh/aliases
source $HOME/.zsh/exports
source $HOME/.zsh/functions
source $HOME/.zsh/function.sh
source $HOME/.zsh/key-binding.zsh
source $HOME/.zsh/fzf

# External
if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi # SwiftEnv
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # NVM
eval `docker-machine env 2>/dev/null` # Docker
eval "$(pyenv init --path)" # Pyenv
eval "$(pyenv virtualenv-init -)" #Pyenv VirtualEnv
if [[ -s "$HOME/.asdf/asdf.sh" ]] ; then source "$HOME/.asdf/asdf.sh" ; fi # ASDF
if [[ -s "$HOME/.asdf/asdf.bash" ]] ; then source "$HOE/asdf/asdf.bash" ; fi # ASDF
if [[ -s "$HOME/.asdf/completions/asdf.sh" ]] ; then source "$HOME/.asdf/completions/asdf.sh" ; fi # ASDF
if [[ -s "$HOME/.rvm/scripts/rvm" ]] ; then source "$HOME/.rvm/scripts/rvm" ; fi # RVM
if which rbenv > /dev/null;
    then eval "$(rbenv init -)";
fi

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    xcape -e 'Caps_Lock=Escape'
fi

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# ZPLUG
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh
zplug "wfxr/forgit"
zplug "spaceship-prompt/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-autosuggestions"
zplug "agkozak/zsh-z"
zplug load

# SPACESHIP
SPACESHIP_VI_MODE_SHOW=false
