# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=0
bindkey -e
eval `dircolors -b`
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/lalman/.zshrc'

autoload -Uz compinit
autoload -U colors && colors
compinit

# End of lines added by compinstall

export EDITOR="emacsclient -c -nw"
export WINEARCH="win32"
export PS1='[%n@%m %1d]$ '

bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char

alias ls='ls --color=auto -F'
alias ll='ls --color=auto -F -al'
alias la='ls --color=auto -F -a'
alias chromium-browser='google-chrome --incognito'
alias dmenu='dmenu -nf white -nb black'
alias urxvt='urxvtc'
alias emacs='emacsclient -c'
alias mg='mg -n'
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
