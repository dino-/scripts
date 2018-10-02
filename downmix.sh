#! /bin/bash

# Script for downmixing a 5.1 (or whatever) video file to two channels (stereo)

inputFile=$(readlink --canonicalize-missing "$1")
dir=$(dirname "$inputFile")
fileWithExt=$(basename "$inputFile")
file="${fileWithExt%.*}"
ext="${fileWithExt##*.}"

ffmpeg -i "$inputFile" -map 0:0 -map 0:1 -map 0:1 -c:v copy -c:a:0 aac -b:a:0 192k -ac 2 -c:a:1 copy "$dir/${file}_ac2.$ext"
