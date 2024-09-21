FROM    opensuse/leap:15.5
RUN     echo 'solver.onlyRequires = true' >> /etc/zypp/zypp.conf
RUN     zypper --non-interactive update; \
        zypper --non-interactive install \
               sudo \
               fish \
               git \
               vim
RUN     zypper --non-interactive update; \
        zypper --non-interactive install \
               python311 \
               python311-pip \
               python311-virtualenv \
               python311-numpy
RUN     groupadd developer
RUN     useradd -m -g developer -s /usr/bin/fish developer
RUN     echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer
RUN     chmod 0440 /etc/sudoers.d/developer
RUN     chsh -s /usr/bin/fish developer
USER    developer
ENV     HOME /home/developer
ENV     PATH "$PATH:$HOME/.local/bin"
ENV     LC_ALL C.UTF-8
RUN     mkdir -pv "$HOME/workspace"
RUN     mkdir -pv "$HOME/.config"
RUN     ln -sv "$HOME/workspace/fish" "$HOME/.config/fish"
RUN     ln -sv "$HOME/workspace/git" "$HOME/.config/git"
RUN     pip3 install --upgrade \
              pip \
              setuptools \
              matplotlib \
              ipykernel \
              jupyter
EXPOSE  8888
WORKDIR /home/developer
VOLUME  /home/developer/workspace
CMD     true
ENTRYPOINT ["/usr/bin/fish", "-c", "jupyter notebook --ip 0.0.0.0 --NotebookApp.token='' --NotebookApp.password=''"]
