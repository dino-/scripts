#! /bin/bash

inputFile="${1:?An input MP4 video must be supplied}"
outputFile="${inputFile%.*}.webm"

ffmpeg \
  -i "$inputFile" \
  -vcodec libvpx \
  -acodec libvorbis \
  -b:v 600k \
  -cpu-used 4 \
  -threads 8 \
  "$outputFile"
