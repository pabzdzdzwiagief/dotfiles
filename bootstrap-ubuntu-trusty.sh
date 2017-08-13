#!/usr/bin/env sh

echo "tmpfs /tmp tmpfs nodev,nosuid,defaults 0 0" | sudo tee -a /etc/fstab
echo """\
description \"Disable swap for faster OOM failures\"
author \"$USER\"

start on runlevel [2345]

task

script
        sudo swapoff -a
end script""" | sudo tee /etc/init/fix-swapoff.conf
echo """\
--- /etc/sysctl.d/10-magic-sysrq.conf   2014-11-08 16:13:55.117276000 +0100
+++ 10-magic-sysrq.conf 2014-11-08 16:13:45.121276000 +0100
@@ -23,4 +23,5 @@
 #   For example, to enable both control of console logging level and
 #   debugging dumps of processes: kernel.sysrq = 10
 #
-kernel.sysrq = 176
+#kernel.sysrq = 176
+kernel.sysrq = 1
""" | sudo patch /etc/sysctl.d/10-magic-sysrq.conf

sudo apt-get -y remove --purge libreoffice-core unity-webapps-common update-notifier-common software-center
sudo apt-get -y update
sudo apt-get -y upgrade

sudo apt-get -y install build-essential git
cd ~/.config/
git clone 'https://pabzdzdzwiagief@github.com/pabzdzdzwiagief/dotfiles'
cd dotfiles/
make install
make install-idea
cd
sudo mkdir /usr/local/share/icons
sudo cp ~/.local/share/idea/bin/idea.png /usr/local/share/icons
cat ~/.Xkbmap | grep option | cut -d' ' -f2 \
  | python -c 'import sys; sys.stdout.write(str(sys.stdin.read().strip().split("\n")))' \
  | xargs -0 dconf write /org/gnome/desktop/input-sources/xkb-options

sudo apt-get -y install zsh tmux curl
sudo chsh -s /usr/bin/zsh $USER

sudo apt-get -y install openbox compton nitrogen
sudo apt-get -y install dmenu parcellite rxvt-unicode
sudo apt-get -y install mpd mpc vlc
sudo update-rc.d mpd disable
mkdir -p ~/.cache/mpd

sudo apt-get -y install emacs
~/.local/bin/emacsd start
~/.local/bin/emacsd stop

cd ~/.config/dotfiles/docker/
sudo apt-get -y install docker.io
sudo gpasswd -a $USER docker
sudo docker build --rm=true -t devbox devbox/
sudo docker run --name devbox-data devbox
