FROM    debian:jessie
ENV     DEBIAN_FRONTEND noninteractive
RUN 	echo 'LANG="C.UTF-8"' > /etc/default/locale
RUN     apt-get update && apt-get install -y locales
RUN     update-locale LANG=C.UTF-8
RUN     update-locale LANGUAGE=C.UTF-8
RUN     apt-get update && apt-get install -y \
          sudo=1.8.10p3-1+deb8u2 \
          apt-transport-https=1.0.9.8
RUN     apt-get update && apt-get install -y \
          man \
          zsh=5.0.7-5 \
          wget=1.16-1 \
          curl=7.38.0-4+deb8u2 \
	  socat=1.7.2.4-2 \
          python=2.7.9-1 \
          build-essential=11.7 \
          git=1:2.1.4-2.1 \
          emacs=46.1 \
          rxvt-unicode=9.20-1+b1 \
          tmux=1.9-6
RUN	echo "deb http://http.debian.net/debian jessie-backports main" > /etc/apt/sources.list.d/jessie-backports.list
RUN     echo "deb https://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list
RUN     apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
RUN     apt-get update && apt-get install -y \
          mercurial=3.1.2-2+deb8u1 \
          python3=3.4.2-2 \
          ipython=2.3.0-2 \
          ipython3=2.3.0-2 \
          python-pip=1.5.6-5 \
          python-virtualenv=1.11.6+ds-1 \
          pypy=2.4.0+dfsg-3 \
          ruby=1:2.1.5+deb8u1 \
          nodejs=0.10.29~dfsg-2 \
          node-underscore=1.7.0~dfsg-1 \
          npm=1.4.21+ds-2 \
          clang=1:3.5-25 \
          cmake=3.0.2-1 \
          golang=2:1.3.3-1 \
          golang-go.tools=0.0~hg20140703-4 \
          openjdk-8-jdk=8u66-b01-1~bpo8+1 \
          openjdk-8-source=8u66-b01-1~bpo8+1 \
          maven=3.0.5-3 \
          gradle=1.5-2 \
          sbt=0.13.9 \
          clojure1.6=1.6.0+dfsg-1
RUN     apt-get update && apt-get install -y \
          aspell=0.60.7~20110707-1.3 \
	  aspell-pl=20140516-1 \
          texlive-full=2014.20141024-2 \
          evince=3.14.1-2
RUN     apt-get -y clean
RUN     curl -sSf https://static.rust-lang.org/rustup.sh > /tmp/rustup.sh
RUN     sh /tmp/rustup.sh -y
RUN     rm -v /tmp/rustup.sh
ENV     PREFIX /usr/local
RUN     wget 'https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein' -O ${PREFIX}/bin/lein
RUN     chmod +x ${PREFIX}/bin/lein
RUN     mkdir -p ${PREFIX}/share/icons
RUN     wget "https://download.jetbrains.com/idea/ideaIC-14.1.4.tar.gz" -O - | tar -C ${PREFIX}/share/ -zxvf -
RUN     mv -v --no-target-directory ${PREFIX}/share/idea-IC-* ${PREFIX}/share/idea
RUN     ln -sv ${PREFIX}/share/idea/bin/idea.png ${PREFIX}/share/icons/idea.png
RUN     ln -sv ${PREFIX}/share/idea/bin/idea.sh ${PREFIX}/bin/idea
RUN     update-alternatives --list java | grep java-8 | xargs update-alternatives --set java
RUN     readlink -f /usr/bin/javac | sed "s:/bin/javac::" | xargs -i echo JAVA_HOME='"{}"' >> /etc/environment
RUN     sed 's/^/export /' /etc/environment | grep JAVA_HOME >> /etc/zsh/zshenv
RUN     groupadd developer
RUN     useradd -m -g developer -s /usr/bin/zsh developer
RUN     echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer
RUN     chmod 0440 /etc/sudoers.d/developer
USER    developer
ENV     HOME /home/developer
ENV     LC_ALL C.UTF-8
WORKDIR /home/developer
VOLUME  /home/developer
CMD     true
ENTRYPOINT ["/usr/bin/zsh", "-c"]
