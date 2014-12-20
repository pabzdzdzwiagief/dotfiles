FROM    ubuntu:14.04
RUN     apt-get update && apt-get install -y software-properties-common
RUN     add-apt-repository ppa:hansjorg/rust
RUN     apt-get update && apt-get install -y --force-yes \
          zsh \
          wget \
          python \
          build-essential \
          git \
          emacs \
          rxvt-unicode \
          tmux \
          ipython \
          ipython3 \
          python-pip \
          python-virtualenv \
          ruby \
          pypy \
          clang \
          rust-nightly \
          golang \
          golang-go.tools \
          cmake \
          scons \
          openjdk-7-jdk \
          openjdk-7-source \
          maven \
          gradle \
          scala \
          clojure1.4 \
          groovy
RUN     groupadd developer
RUN     useradd -m -g developer -s /usr/bin/zsh developer
RUN     echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer
RUN     chmod 0440 /etc/sudoers.d/developer
ADD     dotfiles/ /home/developer/.config/dotfiles/
RUN     chown -R developer:developer /home/developer/.config/
USER    developer
ENV     HOME /home/developer
WORKDIR /home/developer/.config/dotfiles
RUN     make install
RUN     sed -i 's/urxvtc/urxvt/g' bin/tmux-terminal
WORKDIR /home/developer
RUN     .local/bin/emacsd start && .local/bin/emacsd stop
WORKDIR /home/developer/.config/dotfiles
RUN     make idea
WORKDIR /home/developer
RUN     mkdir .local/share/sbt
RUN     wget 'http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.5/sbt-launch.jar' -O .local/share/sbt/sbt-launch.jar
RUN     echo '#!/usr/bin/env sh\n\
              SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"\n\
              exec java $SBT_OPTS -jar $HOME/.local/share/sbt/sbt-launch.jar "$@"' > .local/bin/sbt
RUN     chmod u+x .local/bin/sbt
RUN     .local/bin/sbt
RUN     rm -rf target
RUN     wget 'https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein' -O .local/bin/lein
RUN     chmod u+x .local/bin/lein
RUN     .local/bin/lein
RUN     mkdir workspace
VOLUME  /home/developer
VOLUME  /home/developer/workspace
CMD     true
ENTRYPOINT ["/usr/bin/zsh", "-c"]
