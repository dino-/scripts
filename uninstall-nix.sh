#! /usr/bin/env bash


basename=$(basename "$0")


usage=$(cat <<USAGE
Completely uninstall Nix from a system

usage:
  $basename [OPTIONS]

options:
  --config    Also remove user and root config files
  -h, --help  This help information

Nix is one of those tools that often doesn't install with a distro's proper
packaging. This means it's weird to get rid of it entirely when you want to,
making sure you get all the files and changes it has made. This script will
scrub a Nix installation from a Linux system.

Note that the --config switch will only remove the config files for the user
who invoked this script and the root user's HOME. It may be necessary to track
down other users' config files by-hand.

v1.2  2024-02-22  Dino Morelli <dino@ui3.info>

USAGE
)


warn () {
  echo "$basename:" "$@" >&2
}


die () {
  rc="$1"
  shift
  warn "$@"
  exit "$rc"
}


# arg parsing

getoptResults=$(getopt -o h --long config,help -n "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optRemoveConfig=false
optHelp=false

while true ; do
  case "$1" in
    --config) optRemoveConfig=true; shift;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

if [ $# -gt 0 ]
then
  warn "Extra arguments given"
  die 1 "$usage"
fi

set -x

sudo systemctl stop nix-daemon.service
sudo systemctl disable nix-daemon.socket nix-daemon.service
sudo systemctl daemon-reload

# These are left behind by the arch package after pacman -R
sudo rm /etc/systemd/system/multi-user.target.wants/nix-daemon.service
sudo rm /etc/systemd/system/sockets.target.wants/nix-daemon.socket

rm -rf "$HOME"/{.nix-*,.cache/nix}

localStateDir="$HOME/.local/state"
rm -rf "$localStateDir/nix"
# If the ~/.local/state directory is empty after this it's likely nix made that too, get rid of it
[ -z "$(ls -A $localStateDir)" ] && rm -rf "$localStateDir"

sudo rm -rf /root/{.nix-channels,.nix-defexpr,.nix-profile,.cache/nix}
sudo rm /etc/bash.bashrc.backup-before-nix
sudo rm /etc/profile.d/nix.sh*
sudo rm /etc/tmpfiles.d/nix-daemon.conf
sudo rm -rf /nix

# This is how the official nixos.org installer makes users
sudo sh -c 'for N in $(seq 32); do userdel "nixbld$N"; done'

# and this is how the arch's extra/nix package makes users (only 10 and 0-padded)
sudo sh -c 'for N in $(seq 10); do userdel $(printf "nixbld%02d" $N) ; done'

sudo groupdel nixbld

if $optRemoveConfig
then
  rm -rf "$HOME"/.config/nix
  sudo rm -rf /root/.config/nixpkgs
  sudo rm -rf /etc/nix
fi

set +x

echo "Uninstallation complete."
echo "This script did not modify your $HOME/.bash_profile or $HOME/.profile, there may be nix-specific things there that you need to take care of."
