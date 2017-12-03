FROM    ubuntu:artful
ENV     DEBIAN_FRONTEND noninteractive
RUN 	echo 'LANG="C.UTF-8"' > /etc/default/locale
RUN     apt-get update && \
        apt-get install -y \
          locales=2.26-0ubuntu2 && \
        apt-get clean
RUN     update-locale LANG=C.UTF-8
RUN     update-locale LANGUAGE=C.UTF-8
RUN     locale-gen
RUN     apt-get update && \
        apt-get install -y \
          sudo=1.8.20p2-1ubuntu1 \
          apt-transport-https=1.5 && \
        apt-get clean
RUN     apt-get update && \
        apt-get install -y \
          manpages=4.13-3 \
          fish=2.6.0-1 \
          mercurial=4.3.1-2 \
          git=1:2.14.1-1ubuntu4 \
          curl=7.55.1-1ubuntu2.1 \
          socat=1.7.3.2-1 \
          python2.7 \
          python3.6 \
          build-essential=12.4ubuntu1 \
          scons=2.5.1-1 \
          rxvt-unicode=9.22-1build2 \
          tmux=2.5-3build1 \
          pandoc=1.19.2.1.0-1 \
          texlive-full=2017.20170818-1 \
          aspell=0.60.7~20110707-4 \
	        aspell-pl=20150428-3 \
          dot2tex=2.9.0-2.1 \
          evince=3.26.0-1 && \
        apt-get clean
RUN     apt-get update && \
        apt-get install -y \
          maven=3.5.0-6 && \
        apt-get clean
ADD     https://packages.microsoft.com/keys/microsoft.asc microsoft.asc
RUN     apt-key add microsoft.asc
RUN     echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > \
          /etc/apt/sources.list.d/vscode.list
RUN     apt-get update && \
        apt-get install -y \
          code=1.18.0-1510145176 && \
        apt-get clean
RUN     groupadd developer
RUN     useradd -m -g developer -s /usr/bin/fish developer
RUN     echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer
RUN     chmod 0440 /etc/sudoers.d/developer
USER    developer
ENV     HOME /home/developer
ENV     LC_ALL C.UTF-8
WORKDIR /home/developer
VOLUME  /home/developer
CMD     true
ENTRYPOINT ["/usr/bin/fish", "-c"]
