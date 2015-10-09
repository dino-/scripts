#! /bin/bash

# Do this when your system is new
# It will write a ~/.config/user-dirs.dirs file


function setDir {
   xdg=$1
   dir=$2

   mkdir -p $dir
   xdg-user-dirs-update --set $xdg $dir
}


setDir DESKTOP $HOME
setDir PICTURES $HOME/pictures/misc
setDir VIDEOS $HOME/pictures/video
setDir DOCUMENTS $HOME/doc
setDir TEMPLATES $HOME/doc/templates
setDir DOWNLOAD $HOME/tmp
setDir MUSIC $HOME/tmp/music
setDir PUBLICSHARE $HOME/tmp/share
