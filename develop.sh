#! /usr/bin/env bash


basename=$(basename "$0")

defaultNotesDir="$HOME/doc/notes"


usage=$(cat <<USAGE
Set up tmux environments for development

usage:
  $basename [(-n|--notes-path PATH) | (-e|--edit)]

options:
  -n, --notes-path PATH   Notes file to open, default: ${defaultNotesDir}/{current-directory}_notes.md
                          Ignored if -e|--edit is used.
  -e, --edit              Make an editor-only session, named with an '-edit' suffix
  -h, --help              This help information

Creates tmux sessions for developing in the current directory, and will name
the session using that directory name.

Defaults to starting a tmux session with 3 tabs: git, build, and a tab with a
notes file open in vim if found.

If invoked with 'edit' will make a tmux session intended primarily for editing
without the tabs listed above.

Examples

    $ cd some/project
    $ develop.sh

This will make a tmux session in 'some/project' named 'project'

v1.1  2024-03-06  Dino Morelli <dino@ui3.info>

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

devDir="$(basename $PWD)"

tmuxOptions=(-T 256,focus,title new-session)

if [ $optEdit ]
  then tmuxOptions+=(-s "${devDir}-edit")
  else
    tmuxOptions+=(-s "${devDir}" -n git \; new-window -n build)

    notesPath="${optNotesPath:-${defaultNotesDir}/${devDir}_notes.md}"
    [ -f "$notesPath" ] && tmuxOptions+=(\; new-window -n notes "vim $notesPath")

    tmuxOptions+=(\; select-window -t 0)
fi

tmux "${tmuxOptions[@]}"
