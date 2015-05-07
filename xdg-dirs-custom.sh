#! /bin/bash

# Do this when your system is new

xdg-user-dirs-update --set DESKTOP ~
xdg-user-dirs-update --set PICTURES ~
xdg-user-dirs-update --set VIDEOS ~

mkdir -p ~/doc
xdg-user-dirs-update --set DOCUMENTS ~/doc

mkdir -p ~/doc/templates
xdg-user-dirs-update --set TEMPLATES ~/doc/templates

mkdir -p ~/tmp
xdg-user-dirs-update --set DOWNLOAD ~/tmp
xdg-user-dirs-update --set MUSIC ~/tmp

mkdir -p ~/tmp/share
xdg-user-dirs-update --set PUBLICSHARE ~/tmp/share
