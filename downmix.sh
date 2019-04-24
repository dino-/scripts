#! /bin/bash

# Script for downmixing a 5.1 (or whatever) video file to two channels (stereo)


set -e

# We want to write the new file in the same directory as the old file, so use
# absolute paths from here on out
inputFile=$(splitpath --makeabsolute "$1")

# Isolate just the path/file part without extension
pathFile=$(splitpath --dropextension "$inputFile")

# And get the extension for use below
extension=$(splitpath --takeextension "$inputFile")

# Construct our new filename with '_ac2' right before the extension. This is
# supposed to mean Audio Channels 2.
outputFile="${pathFile}_ac2${extension}"

# Perform the transcoding
ffmpeg -i "$inputFile" -map 0:0 -map 0:1 -map 0:1 -c:v copy -c:a:0 aac -b:a:0 192k -ac 2 -c:a:1 copy "$outputFile"
