#!/usr/bin/env sh

cd ~/.config/autostart

run-desktop "/etc/xdg/autostart/gnome-settings-daemon.desktop" &

for program in *.desktop
do
    run-desktop "$program" &
done

# to keep processes nicely grouped under this script
wait
