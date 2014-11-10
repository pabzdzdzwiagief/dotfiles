FROM    devbox
USER    root
RUN     apt-get update && apt-get install -y \
          openjdk-7-jdk \
          openjdk-7-source \
          maven \
          leiningen \
          gradle \
          scala \
          clojure1.4 \
          groovy
USER    developer
WORKDIR /home/developer
RUN     cd .config/dotfiles && make idea
ENTRYPOINT ["/usr/bin/zsh", "-c"]
CMD     ["tmux-terminal"]
