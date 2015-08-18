set --export PATH $PATH $HOME/.local/bin
set --export VISUAL emacsc
set --export EDITOR ex
if set -q XDG_CONFIG_HOME
   set --export HGRCPATH $XDG_CONFIG_HOME/hg/hgrc
else
   set --export HGRCPATH $HOME/.config/hg/hgrc
end
alias ls 'ls --color=auto -F'
alias cp 'cp -i'
alias mv 'mv -i'
alias rm 'rm -i'
