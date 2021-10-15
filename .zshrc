fpath=(
  $fpath
  ~/.rvm/scripts/zsh/Completion
  ~/.zsh/functions
  /usr/local/share/zsh/site-functions
)

# Theme
ZSH_THEME="robbyrussell"

# ZStyle
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"
zstyle ':completion:*:ssh:*' tag-order hosts users
zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zshcache

# Autoload
autoload colors; colors
autoload -U compinit; compinit
autoload edit-command-line
zle -N edit-command-line

# Keybinds
bindkey -e
bindkey '^x^e' edit-command-line
bindkey '\ep' up-line-or-search
bindkey '\en' down-line-or-search
bindkey '\ew' kill-region

if [ -z "$TMUX" ]; then
  fg-widget() {
    stty icanon echo pendin -inlcr < /dev/tty
    stty discard '^O' dsusp '^Y' lnext '^V' quit '^\' susp '^Z' < /dev/tty
    zle reset-prompt
    if jobs %- >/dev/null 2>&1; then
      fg %-
    else
      fg
    fi
  }

  zle -N fg-widget
  bindkey -M emacs "^Z" fg-widget
  bindkey -M vicmd "^Z" fg-widget
  bindkey -M viins "^Z" fg-widget
fi

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

# Plugins
plugins=(rails git ruby bundler vi-mode z)

# Functions
l.() {
  ls -ld "${1:-$PWD}"/.[^.]*
}

emacs.() {
  emacsclient -c -a= $*
}

# default apps
(( ${+PAGER}   )) || export PAGER='less'
(( ${+EDITOR}  )) || export EDITOR='vim'
export PSQL_EDITOR='vim'

# Aliases
# * Additional aliases are found in `.sharedrc`
#
alias reload='source ~/.zshrc; echo -e "\n\u2699  \e[33mZSH config reloaded\e[0m \u2699"'

cuke() {
  local file="$1"
  shift
  cucumber "features/$(basename $file)" $@
}
compctl -g '*.feature' -W features cuke

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
source $ZSH/oh-my-zsh.sh # Oh-My-ZSH

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

command -v brew > /dev/null && [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/bshega/.nvm/versions/node/v9.8.0/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/bshega/.nvm/versions/node/v9.8.0/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/bshega/.nvm/versions/node/v9.8.0/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/bshega/.nvm/versions/node/v9.8.0/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# ZPLUG
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh
zplug "wfxr/forgit"
