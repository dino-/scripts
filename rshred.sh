#! /usr/bin/env bash

# recursively shred an entire directory tree
#
# 1. recursively visit all files in the directory, shredding each one individually
# 2. count remaining files
# 3. report on remaining files OR recursively remove the (now empty) directories

shredBinary="shred"

command -v "$shredBinary" &> /dev/null ||
  {
    echo "Can't continue because this script requires '$shredBinary' on the PATH"
    exit 2
  }

dir=${1:?"Can't continue because no target directory was supplied to shred"}

[ -d "$dir" ] ||
  { echo "Can't continue because directory '$dir' doesn't exist"; exit 1; }

find "$dir" -type f -exec "$shredBinary" -u {} \;

filesRemaining=$(find "$dir" -type f | wc -l)
# echo $filesRemaining

if [ "$filesRemaining" -eq "0" ]
then
  rm -rfv "$dir"
else
  echo "$filesRemaining files remain!"
  exit 1
fi
