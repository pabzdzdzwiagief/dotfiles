FROM    ubuntu:trusty
RUN     apt-get update && apt-get install -y software-properties-common
RUN     add-apt-repository -y ppa:ubuntu-wine/ppa
RUN     dpkg --add-architecture i386
ENV     DEBIAN_FRONTEND noninteractive
RUN     apt-get update && apt-get install -y \
          man \
          wget \
          curl
RUN     apt-get update && apt-get install -y \
          dbus-x11 \
          pulseaudio
RUN     apt-get update && apt-get install -y \
          wine1.7 \
          winetricks
RUN     apt-get update && apt-get install -y dosbox
RUN     apt-get update && apt-get install -y libsdl2-2.0-0 # for Warsow
RUN     apt-get update && apt-get install -y locales
RUN     update-locale LANG=C.UTF-8 LC_MESSAGES=POSIX
RUN     useradd -m -g games -G audio,video,users -s /bin/dash player
RUN     echo "player ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/player
RUN     chmod 0440 /etc/sudoers.d/player
USER    player
ENV     HOME /home/player
WORKDIR /home/player
RUN     echo "export PULSE_SERVER=/run/pulse/native" >> $HOME/.profile
VOLUME  /home/player
