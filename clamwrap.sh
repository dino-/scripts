#! /usr/bin/env sh

# A script to fill in some good defaults for clamscan that are often needed


basename=$(basename "$0")


usage=$(cat <<USAGE
Wrapper script to add some useful defaults to clamscan

usage:
  $basename [OPTIONS]
  $basename [OPTIONS] [-- CLAMSCAN-OPTIONS] [DIR1] [DIR2] [FILE1] ...

options:
  -a, --arch-packages   Scan Arch Linux pacman packages modified in the past day
  -d, --no-update-defs  Do not update virus defs with 'freshclam'
  -h, --help            This help information

frequently-used clamscan options:
  -r, --recursive  Scan subdirectories recursively
  -i, --infected   Only print infected files

No directory or file implies . unless the -a switch is used

For more clamscan switches: $ clamscan --help

Most common usages:

  $ clamwrap.sh -a
  $ clamwrap.sh -- -ri

INFECTION FALSE-POSITIVES

If you get reports of a file or files infected with Unix.Trojan.Mirai, it may
be a false-positive with the local clamav. Try checking the files with an
online scanner like https://www.virustotal.com

v1.3  2023-04-29  Dino Morelli <dino@ui3.info>

USAGE
)


die () {
  rc="$1"
  shift
  echo "$basename:" "$@" >&2
  exit "$rc"
}


# arg parsing

getoptResults=$(getopt -o adh --long arch-packages,no-update-defs,help -n "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optArchPackages=false
optUpdateDefs=true
optHelp=false

while true ; do
  case "$1" in
    -a|--arch-packages) optArchPackages=true; shift;;
    -d|--no-update-defs) optUpdateDefs=false; shift;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

$optUpdateDefs && sudo freshclam

packagesDir="/var/lib/pacman/local"

scanCommand="clamscan --bytecode-timeout=300000 --max-filesize=4000M --max-scansize=4000M $@"

if $optArchPackages
  then
    set -x

    cd "$packagesDir" || die 1 "Couldn't cd into $packagesDir"
    find . -type d -mtime -1 -not -name '.' | xargs $scanCommand
  else
    set -x
    $scanCommand
fi
