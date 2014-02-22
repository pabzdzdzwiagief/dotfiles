dotfiles
========

Scripts and configuration files for:
- emacs
- parcellite
- i3
- urxvt
- mpd
- tmux
- zsh

installation
------------

Clone this repo somewhere (e.g. in `~/.config`) and run `make install` to
create symlinks inside the `$HOME` directory. Run `chsh -s $(which zsh)` to set
`zsh` as your default shell.

Installation requirements:
- GNU Make
- GNU Wget
- Python

license
-------

All files are under the permissive
[CC0](https://creativecommons.org/publicdomain/zero/1.0/) license unless
stated otherwise.
