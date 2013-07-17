!/usr/bin/env sh

cd ~/.config/autostart

for program in "gnome-settings-daemon" \
               "gnome-keyring-daemon"  \
               "nm-applet"
do
    run-desktop "/etc/xdg/autostart/$program.desktop" &
done

for program in *.desktop
do
    run-desktop "$program" &
done

# to keep processes nicely grouped under this script
wait
