#! /usr/bin/env bash


basename=$(basename "$0")


usage=$(cat <<USAGE
Remove useless Apple OSX files and dirs

usage:
  $basename [OPTIONS] [FILE_OR_DIR]

options:
  -h, --help       This help information

When you get a zip file made by a Mac user, often it will contain some Apple
metadata including a '__MACOSX' directory and some files with names ending in
'.DS_Store'. Completely useless for anyone who doesn't use a Mac.

Usage examples

  $basename           Clean up the current directory (.)
  $basename DIR       Clean up in DIR
  $basename ZIPFILE   Clean up ZIPFILE in-place


v1.0  2025-01-30  Dino Morelli <dino@ui3.info>

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

getoptResults=$(getopt --options h --longoptions help --name "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optHelp=false

while true ; do
  case "$1" in
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

if [ $# -gt 1 ]; then
  warn "Zero or one argument expected"
  die 1 "$usage"
fi

if [ $# -eq 0 ]; then
  target="."
else
  target="$1"
fi

if [ -f "$target" ]; then
  # shellcheck disable=SC2035
  zip -d "$target" __MACOSX* *.DS_Store
else
  cd "$target" || die 1 "Unable to cd to $target"
  rm -rfv __MACOSX
  find . -name '.DS_Store' -print -delete
fi
