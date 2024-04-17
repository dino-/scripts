#! /usr/bin/env bash


basename=$(basename "$0")

defaultNotesDir="$HOME/doc/notes"
devDir="$(basename $PWD)"


usage=$(cat <<USAGE
Set up tmux environments for development

usage:
  $basename [(-n|--notes-path PATH) | (-e|--edit)] [NAME]

options:
  -n, --notes-path PATH   Notes file to open, default: ${defaultNotesDir}/{current-directory}_notes.md
  -e, --edit              Make an editor-only session, named with an '-edit' suffix
  NAME                    Optional name for this tmux session. Default: ${devDir} (current working dir)
  -h, --help              This help information

Creates tmux sessions for developing in the current directory, and will name
the session using that directory name.

Defaults to starting a tmux session with 3 tabs: git, build, and a tab with a
notes file open in vim if found.

When invoked with -e|--edit this script will make a tmux session intended
primarily for editing without the tabs listed above.

Examples

    $ cd some/project
    $ develop.sh

This will make a tmux session in 'some/project' named 'project'

v1.3  2024-04-17  Dino Morelli <dino@ui3.info>

USAGE
)


die () {
  rc="$1"
  shift
  echo "$basename:" "$@" >&2
  exit "$rc"
}


# arg parsing

getoptResults=$(getopt --options en:h --longoptions edit,notes-path:,help --name "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

opeEdit=false
optHelp=false

while true ; do
  case "$1" in
    -e|--edit) optEdit=true; shift;;
    -n|--notes-path) optNotesPath=$2; shift; shift;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

tmuxOptions=(-T 256,focus,title new-session)

sessionName=${1:-$devDir}

if [ $optEdit ]
  then tmuxOptions+=(-s "${sessionName}-edit")
  else
    tmuxOptions+=(-s "${sessionName}" -n git \; new-window -n build)

    notesPath="${optNotesPath:-${defaultNotesDir}/${sessionName}_notes.md}"
    [ -f "$notesPath" ] && tmuxOptions+=(\; new-window -n notes "vim $notesPath")

    tmuxOptions+=(\; select-window -t 0)
fi

tmux "${tmuxOptions[@]}"
