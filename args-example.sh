#! /bin/bash


basename=$(basename "$0")


usage=$(cat <<USAGE
Argument parsing demo script

usage:
  $basename [OPTIONS] ITEM1 ITEM2 ...

options:
  -a, --arg-a      An argument named a. Default false
  -b, --arg-b INT  An argument named b
  -h, --help       This help information

v1.2  2020-09-24  Dino Morelli <dino@ui3.info>

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

getoptResults=$(getopt -o ab:h --long arg-a,arg-b:,help -n "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

optA=false
optHelp=false

while true ; do
  case "$1" in
    -a|--arg-a) optA=true; shift;;
    -b|--arg-b) optB="$2"; shift 2;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

echo "detail of arguments"
echo "optA: $optA"
echo "optB: $optB"
echo "optHelp: $optHelp"
echo "number of remaining parameters: $#"
echo "remaining parameters: $*"
echo

$optHelp && die 0 "$usage"

if [ $# -lt 2 ]
then
  warn "Incorrect number of ITEMs"
  die 1 "$usage"
fi
