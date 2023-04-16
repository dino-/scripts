#! /usr/bin/env sh


basename=$(basename "$0")


usage=$(cat <<USAGE

Virus scan of the Arch Linux pacman packages modified on the system in the past
day. This is to be run after 'pacman -Syu' or similar

usage:
  $basename [OPTIONS] ITEM1 ITEM2 ...

options:
  -d, --no-update-defs  Do not update virus defs with 'freshclam'
  -h, --help            This help information

v1.0  2023-04-16  Dino Morelli <dino@ui3.info>

USAGE
)


die () {
  rc="$1"
  shift
  echo "$basename:" "$@" >&2
  exit "$rc"
}


# arg parsing

getoptResults=$(getopt -o dh --long no-update-defs,help -n "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optUpdateDefs=true
optHelp=false

while true ; do
  case "$1" in
    -d|--no-update-defs) optUpdateDefs=false; shift;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

$optUpdateDefs && sudo freshclam

packagesDir="/var/lib/pacman/local"

set -x

cd "$packagesDir" || die 1 "Couldn't cd into $packagesDir"
find . -type d -mtime -1 -not -name '.' \
  | xargs clamscan --bytecode-timeout=300000 --max-filesize=4000M --max-scansize=4000M
