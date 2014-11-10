FROM    ubuntu:14.04
RUN     apt-get update && apt-get install -y \
          zsh \
          wget \
          python \
          build-essential \
          git \
          emacs \
          rxvt-unicode \
          tmux
RUN     groupadd developer
RUN     useradd -m -g developer -s /usr/bin/zsh developer
RUN     echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer
RUN     chmod 0440 /etc/sudoers.d/developer
USER    developer
ENV     HOME /home/developer
WORKDIR /home/developer
RUN     mkdir -p .config
RUN     cd .config && git clone 'https://pabzdzdzwiagief@github.com/pabzdzdzwiagief/dotfiles'
RUN     cd .config/dotfiles && make install
RUN     sed -i 's/urxvtc/urxvt/g' .config/dotfiles/bin/tmux-terminal
RUN     .local/bin/emacsd start && .local/bin/emacsd stop
ENTRYPOINT ["/usr/bin/zsh", "-c"]
CMD     ["tmux-terminal"]
