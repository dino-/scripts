#! /bin/bash


# These will be specific to where you have your phone mount and what
# reader software is used on the phone
mountPoint=/media/mytouch
dest=$mountPoint/eBooks/import


case "$1" in
   -h|--help)
      echo "usage: bookinstall.sh EPUB_FILES"
      echo "       bookinstall.sh -h"
      echo "       bookinstall.sh --help"
      exit 1
      ;;
esac


if [ "$1" == "" ]
   then
      echo "No book file paths were supplied for installation."
      echo "FAILURE"
      exit 1
fi


# Make sure the phone is mounted

mount | grep -q $mountPoint
alreadyMounted=$?

if [ "$alreadyMounted" == "0" ]
   then
      echo "Phone already mounted"
      successfulMount=0
   else
      echo "Phone not mounted, mounting now"
      mount $mountPoint
      successfulMount=$?
fi

# If unsuccessful, say so and exit 1
if [ "$successfulMount" != "0" ]
   then
      echo "There has been a problem mounting $mountPoint"
      echo "FAILURE"
      exit 1
fi


# Copy the book files

cp -v $* $dest


# Clean up the mount if necessary

if [ "$alreadyMounted" == "0" ]
   then
      echo "WARNING: Phone was not unmounted!"
      echo "You MUST do this before unplugging!"
   else
      echo "Unmounting phone now..PLEASE WAIT..."
      umount $mountPoint
      echo "Done, you may now unplug"
fi
