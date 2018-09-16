#! /bin/bash

# Script for downmixing a 5.1 (or whatever) video file to two channels (stereo)

inputFile=$(readlink --canonicalize-missing "$1")
dir=$(dirname "$inputFile")
fileWithExt=$(basename "$inputFile")
file="${fileWithExt%.*}"
ext="${fileWithExt##*.}"

ffmpeg -i "$inputFile" -ac 2 "$dir/${file}_ac2.$ext"
