#! /usr/bin/env bash

# This script will result in the creation of a
# ~/.config/user-dirs.dirs file

# It's also great to do when your system is new, before you start
# getting those annoying Mac/Windows-like directories like Desktop
# and Videos


# When xdg-user-dirs-update is run automatically at login, it
# behaves in a dumb manner if any of the directories you have set
# don't exist. It resets to the system default path and dorks your
# user-dirs.dirs file.
#
# To avoid that, we'll create these directories at the same time
# that we set them.

function setDir {
   xdg=$1
   dir=$2

   mkdir -p "$dir"
   xdg-user-dirs-update --set "$xdg" "$dir"
}


setDir DESKTOP "$HOME/.local/share/desktop"
setDir PICTURES "$HOME/pictures/misc"
setDir VIDEOS "$HOME/pictures/video"
setDir DOCUMENTS "$HOME/doc"
setDir TEMPLATES "$HOME/doc/templates"
setDir DOWNLOAD "$HOME/tmp"
setDir MUSIC "$HOME/sound/music"
setDir PUBLICSHARE "$HOME/tmp/share"
