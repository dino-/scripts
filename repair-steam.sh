#! /usr/bin/env bash


basename=$(basename "$0")


usage=$(cat <<USAGE
$basename - Fix broken Linux native Steam client

usage:
  $basename [OPTIONS]

options:
  -n, --no-action  Don't delete anything, show what would be done
  --serious        Only leave behind the ssfn* files and steamapps/ directory
                   Use this when "normal mode" doesn't work
  -h, --help       This help information

This script (carefully!) deletes some Steam client state while leaving the
binary installed and preserving your installed games. It can fix some problems
that crop up periodically when the client is updated.

This script will keep all installed games and (usually?) user settings but you
will be required to log into Steam again.

Make sure your Steam client is not running before running this.

Problems this script may address:

- All top-level tabs except the Library get stuck infinitely loading
- Just the Library tab shows a blank gray background and never loads any
  content

The procedure is this: We want to delete everything in
\$HOME/.local/share/Steam/ except for:

    ssfn*
    steamapps/
    skins/
    userdata/

If the --serious switch is used, only this will be kept:

    ssfn*
    steamapps/

Be VERY CAREFUL with find commands like the one in this script, very dangerous!
In fact, not bad to have a good backup before running this.

v1.3  2023-11-13  Dino Morelli <dino@ui3.info>

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

getoptResults=$(getopt -o n,h --long no-action,serious,help -n "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optDryRun=false
optSerious=false
optHelp=false

while true ; do
  case "$1" in
    -n|--no-action) optDryRun=true; shift;;
    --serious) optSerious=true; shift;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

pgrep -ax steam > /dev/null \
  && die 1 "Can't continue because Steam appears to be running"


if $optSerious; then
  exclusions="! -name 'Steam' ! -name 'ssfn*' ! -name 'steamapps'"
else
  exclusions="! -name 'Steam' ! -name 'ssfn*' ! -name 'steamapps' ! -name 'skins' ! -name 'userdata'"
fi

if $optDryRun; then
  findCommand="-print"
else
  findCommand="-exec rm --recursive --force --verbose {} \;"
fi

homeSteamDir="$HOME/.local/share/Steam"

[[ -d "$homeSteamDir" ]] \
  || die 1 "Aborting because Steam directory $homeSteamDir doesn't exist"

set -x

eval "find $homeSteamDir -maxdepth 1 $exclusions $findCommand"
