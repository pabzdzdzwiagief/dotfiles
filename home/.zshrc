# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=0
bindkey -e
eval `dircolors -b`
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
setopt histignoredups
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
autoload -U colors && colors
autoload -U promptinit
compinit
promptinit

zstyle ':completion:*' menu select
prompt redhat

# End of lines added by compinstall

DIRSTACKSIZE=20
setopt autopushd pushdsilent pushdtohome
setopt pushdignoredups

alias ls='ls --color=auto -F'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias grep='grep -i --color --null'

case $TERM in
    screen|xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
        precmd () {print -Pn "\e]0;%n@%m: %~\a"}
        chpwd() {print -Pn "\e]0;%n@%m: %~\a"}
        ;;
esac
