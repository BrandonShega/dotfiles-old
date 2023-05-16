# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

fpath=(
  $fpath
  ~/.rvm/scripts/zsh/Completion
  ~/.zsh/functions
  /usr/local/share/zsh/site-functions
  "$(brew --prefix)/share/zsh/site-functions"
)

# Autoload
autoload colors; colors
autoload -Uz compinit && compinit
autoload edit-command-line
autoload -U promptinit; promptinit
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
setopt nocorrectall
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
(( ${+EDITOR}  )) || export EDITOR='vim'
export PSQL_EDITOR='vim'

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
. $(brew --prefix asdf)/libexec/asdf.sh
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

# SPACESHIP
SPACESHIP_VI_MODE_SHOW=false

# COMPLETION
# case insensitive path-completion 
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
# partial completion suggestions
zstyle ':completion:*' list-suffixes zstyle ':completion:*' expand prefix suffix

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/z-a-rust \
    zdharma-continuum/z-a-as-monitor \
    zdharma-continuum/z-a-patch-dl \
    zdharma-continuum/z-a-bin-gem-node

### End of Zinit's installer chunk

# Zinit PLugins
# zinit light spaceship-prompt/spaceship-prompt
zinit load wfxr/forgit
zinit load zsh-users/zsh-syntax-highlighting
zinit load zsh-users/zsh-autosuggestions
zinit load agkozak/zsh-z
zinit load marlonrichert/zsh-autocomplete
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

# prompt pure

# eval "$(starship init zsh)"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
