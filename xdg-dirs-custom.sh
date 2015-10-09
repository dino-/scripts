#! /bin/bash

# Do this when your system is new

xdg-user-dirs-update --set DESKTOP ~

mkdir -p ~/pictures/misc
xdg-user-dirs-update --set PICTURES ~/pictures/misc

mkdir -p ~/pictures/video
xdg-user-dirs-update --set VIDEOS ~/pictures/video

mkdir -p ~/doc
xdg-user-dirs-update --set DOCUMENTS ~/doc

mkdir -p ~/doc/templates
xdg-user-dirs-update --set TEMPLATES ~/doc/templates

mkdir -p ~/tmp
xdg-user-dirs-update --set DOWNLOAD ~/tmp

mkdir -p ~/tmp/music
xdg-user-dirs-update --set MUSIC ~/tmp/music

mkdir -p ~/tmp/share
xdg-user-dirs-update --set PUBLICSHARE ~/tmp/share
