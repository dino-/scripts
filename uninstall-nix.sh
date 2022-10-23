#! /usr/bin/env bash


basename=$(basename "$0")


usage=$(cat <<USAGE
Completely uninstall Nix from a system

usage:
  $basename [OPTIONS]

options:
  --config    Also remove your user config directory
  -h, --help  This help information

Nix is one of those tools that often doesn't install with a distro's proper
packaging. This means it's weird to get rid of it entirely when you want to,
making sure you get all the files and changes it has made. This script will
scrub a Nix installation from a Linux system.

Note that the --config switch will only remove the config files for the user
who invoked this script. It may be necessary to track down other users' config
files by-hand.

v1.1  2022-10-23  Dino Morelli <dino@ui3.info>

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

sudo systemctl disable --now nix-daemon.service
sudo systemctl disable --now nix-daemon.socket

rm -rf "$HOME"/{.nix-*,.cache/nix}
sudo rm -rf /root/{.nix-channels,.nix-defexpr,.nix-profile,.cache/nix}
sudo rm -rf /etc/profile.d/nix.sh*
sudo rm -rf /nix

sudo sh -c 'for N in $(seq 32); do userdel "nixbld$N"; done'

if $optRemoveConfig
then
  rm -rf "$HOME"/.config/nix
  sudo rm -rf /root/.config/nixpkgs
  sudo rm -rf /etc/nix
fi

echo "Uninstallation complete."
echo "This script did not modify your $HOME/.bash_profile or $HOME/.profile, there may be nix-specific things there that you need to take care of."
