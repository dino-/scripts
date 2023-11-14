#! /usr/bin/env bash


basename=$(basename "$0")


function usage {
  cat <<USAGE
$basename - Locate the screen session a process is within

usage:
  $basename [OPTIONS] PID

options:
  -h, --help  This help information

Here's the use case for this:

You open a file in Vim and it tells you a Vim swapfile already exists, and the
process is also still running. It's another instance of Vim on this system. If
that instance is inside a gnu screen session and you have many of those, it can
be a pain in the ass to switch to or detach/attach each one and check each tab
to locate it.

Gnu screen puts the session PID.NAME in an environment variable called STY and
this is inherited by everything run under that screen session. This script uses
'ps wwep PID' to locate the full environment of a process id and then pipes
that output to "grep -o 'STY=\\S*'" to pull out and show you any STY variable
in its environment.

So, you plug the PID from the Vim warning message into this script and get a
screen session name, that's where the Vim is running, you can now reattach to
that to continue editing.

v1.1  2023-11-13  Dino Morelli <dino@ui3.info>

USAGE
}


# arg parsing

getoptResults=$(getopt -o h --long help -n "$basename" -- "$@") \
  || { usage; exit 1; }

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optHelp=false

while true ; do
  case "$1" in
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && { usage; exit 0; }

if [ $# -lt 1 ]
then
  echo "Missing required PID argument"
  usage
  exit 1
fi

# Can't use pgrep here because we need the unlimited-width process output
# containing the environment. Also, we don't need to locate the process id, we
# already have it.
# shellcheck disable=SC2009
ps wwep "$1" | grep -o 'STY=\S*'
