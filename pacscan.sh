#! /usr/bin/env sh

# Virus scan of the Arch Linux pacman packages modified on the system in the
# past day
# This is to be run after `pacman -Syu` or similar


die () {
  rc="$1"
  shift
  echo "$basename:" "$@" >&2
  exit "$rc"
}

packagesDir="/var/lib/pacman/local"

set -x

cd "$packagesDir" || die 1 "Couldn't cd into $packagesDir"
find . -type d -mtime -1 -not -name '.' \
  | xargs clamscan --bytecode-timeout=300000 --max-filesize=4000M --max-scansize=4000M
