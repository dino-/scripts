#! /bin/bash

# Some useful optional switches:
#   -silent       When running from cron to suppress normal output
#   -batch=false  To manually resolve conflicts
#   -debug=all    For lots of debug output
#   -force=/home/dino/doc/cloud  To force the local copy to be copied to the remote


basename=$(basename "$0")

die () {
  rc="$1"
  shift
  echo "$basename:" "$@" >&2
  exit "$rc"
}


# This script relies on another tool, and is routinely run from
# a systemd timer or cron job, with a sketchy env! Be sure we can locate
# things:
specialhome=/home/dino
# withmount="${specialhome}/dev/scripts/withmount.hs"

#flock -n "${specialhome}/var/run/lock/unison.lock" unison $* -ui text cloud
# Having problems with idiot Unison and idiot OCaml. Version incompatibility between what's on Arch Linux and what's on Debian. Falling back to mounting the remote storage locally so it's ONE copy of Unison doing everything
# $withmount /media/cloud flock -n "${specialhome}/var/run/lock/unison-cloud.lock" unison "$@" -ui text cloud

mountPoint="/media/cloud"

# Note: 'cloud:' here refers to a remote system configured in $HOME/.config/rclone/rclone.conf
rclone mount --daemon cloud:doc/cloud "$mountPoint" \
  || die 1 "Can't continue because the rclone mount failed"

flock -n "${specialhome}/var/run/lock/unison-cloud.lock" unison "$@" -ui text cloud

fusermount -u "$mountPoint"
