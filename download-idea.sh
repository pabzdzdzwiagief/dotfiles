#!/usr/bin/env sh

wget "http://download-ln.jetbrains.com/idea/ideaIC-12.1.4.tar.gz" -O - | \
tar -C ~/.local/share/ -zxvf -
mv ~/.local/share/idea-IC-129.713 ~/.local/share/idea
patch -p1 ~/.local/share/idea/bin/idea.sh < idea/idea.sh.patch
