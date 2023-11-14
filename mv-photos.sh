#! /usr/bin/env bash


# Everything we do from here is relative to this directory
cd $HOME/doc/cloud/mobile/phone/camera

# We need to perform this command more than once
findcmd="find . -name '*.jpg' -links 1 -print"

# If we have no files, get out of here now
test -z "$(eval $findcmd -quit)" && exit 0

# We're really doing something, log the date
date +"%F %T"

# Use the find command to really rename the files this time
eval $findcmd | xargs photoname -p $HOME/pictures/photos

# A blank line to separate this from the next log entries
echo
