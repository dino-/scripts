#! /bin/bash


basename=$(basename $0)


function usage {
   cat <<USAGE
$basename - Argument parsing demo script

usage:
   $basename [OPTIONS] ITEM1 ITEM2 ...

options:
   -a, --arg-a       An argument named a. Default false
   -b, --arg-b INT   An argument named b
   -h, --help        This help information

v1.0  2016-02-17  Dino Morelli <dino@ui3.info>

USAGE
}


# arg parsing

getoptResults=$(getopt -o ab:h --long arg-a,arg-b:,help -n $basename -- "$@")

if [ $? != 0 ]; then usage; exit 1; fi

# Note the quotes around '$getoptResults': they are essential!
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
echo "remaining parameters: $@"
echo

if $optHelp; then usage; exit 0; fi

if [ $# -lt 2 ]
then
   echo "Incorrect number of ITEMs"
   usage
   exit 1
fi
