FROM    devbox
USER    root
RUN     apt-get update && apt-get install -y \
          openjdk-7-jdk \
          openjdk-7-source \
          maven \
          gradle \
          scala \
          clojure1.4 \
          groovy
USER    developer
WORKDIR /home/developer
RUN     cd .config/dotfiles && git pull
RUN     cd .config/dotfiles && make idea
RUN     wget 'https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein' -O .local/bin/lein
RUN     chmod +x .local/bin/lein
RUN     .local/bin/lein
ENTRYPOINT ["/usr/bin/zsh", "-c"]
CMD     ["tmux-terminal"]
