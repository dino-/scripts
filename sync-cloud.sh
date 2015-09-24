#! /bin/bash

# Use the additional -silent switch when running from cron

#flock -n /home/dino/var/run/lock/unison.lock unison $* -ui text cloud
# Having problems with idiot Unison and idiot OCaml. Version incompatibility between what's on Arch Linux and what's on Debian. Falling back to mounting the remote storage locally so it's ONE copy of Unison doing everything
/home/dino/bin/withmount.hs /media/cloud flock -n /home/dino/var/run/lock/unison-cloud.lock unison $* -ui text cloud


# These are from the now-abandoned Dump Truck service
#/home/dino/bin/withmount.hs /media/dav/dumptruck flock -n /home/dino/var/run/lock/unison.lock unison $* -ui text cloud
#/home/dino/bin/withmount.hs /media/dav/dumptruck flock -n /home/dino/var/run/lock/unison.lock unison $* -ui text cloud 2>/home/dino/var/log/unison$(date +"%Y%m%d-%H%M").err
