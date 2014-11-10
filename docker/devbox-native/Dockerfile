FROM    devbox
USER    root
RUN     apt-get update && apt-get install -y software-properties-common
RUN     add-apt-repository ppa:hansjorg/rust
RUN     apt-get update && apt-get install -y --force-yes \
          clang \
          rust-nightly \
          golang \
          golang-go.tools \
          cmake \
          scons \
          ipython \
          ipython3
USER    developer
WORKDIR /home/developer
ENTRYPOINT ["/usr/bin/zsh", "-c"]
CMD     ["tmux-terminal"]
