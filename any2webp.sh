#! /usr/bin/env bash


basename=$(basename "$0")


optHelp=false
optQuality="90"
optQuiet=false
optSuffix=""

usage=$(cat <<USAGE
Convert JPG, PNG and GIF image files to WEBP with some default settings

usage:
  $basename [OPTIONS] SRCFILE [DESTFILE]

options:
  -q, --quality INT  JPG to WEBP quality setting, default $optQuality
  -s, --suffix STR   Text to be placed in the DEST filename before the '.webp'
                     extension. Ignored if a DESTFILE argument was specified.
  -Q, --quiet        Don't echo the convert command
  -h, --help         This help information

JPG files will be converted with the quality specified or the default.

PNG and GIF files will be converted losslessly.

If DESTFILE is omitted, SRCFILE will be used with the extension changed to '.webp'

v1.3  2025-03-03  Dino Morelli <dino@ui3.info>

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

getoptResults=$(getopt -o q:Qs:h --long quality:,quiet,suffix:,help -n "$basename" -- "$@") \
  || die 1 "$usage"

# Note the quotes around "$getoptResults": they are essential!
eval set -- "$getoptResults"

while true ; do
  case "$1" in
    -q|--quality) optQuality="$2"; shift 2;;
    -Q|--quiet) optQuiet=true; shift;;
    -s|--suffix) optSuffix="$2"; shift 2;;
    -h|--help) optHelp=true; shift;;
    --) shift; break;;
  esac
done

$optHelp && die 0 "$usage"

if [ $# -lt 1 ]
then
  warn "No input file specified"
  die 1 "$usage"
fi

inputImage="$1"

filename="${inputImage%.*}"
extension="${inputImage##*.}"

outputImage=${2:-${filename}${optSuffix}.webp}

[ -e "$outputImage" ] && die 1 "Output file $outputImage already exists!"


case "$extension" in
  jpg|JPG|jpeg|JPEG) switches=(-quality "$optQuality");;
  png|PNG|gif|GIF|tiff|TIFF) switches=(-define webp:lossless=true);;
  *) die 1 "Unknown file type: $inputImage";;
esac

$optQuiet || set -x

magick "$inputImage" "${switches[@]}" "$outputImage"
