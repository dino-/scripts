#! /bin/sh

# Script for multiple rsync backups to one dest
# Dino Morelli <dino@ui3.info>  2008-01-17
#
# http://ui3.info/d/proj/bak.html
# Installation instructions at end of this script


# Switches used by all rsync invocations
# Another useful one may be -n, --dry-run
#commonSwitches="-av -R --delete"
#commonSwitches="--archive --verbose --relative --delete"
commonSwitches="--archive --verbose --relative --delete --dry-run"

destPath="user@host.blah:/top/level/remote/backup/dir"

# This one redirects all errors to stdout, but otherwise lets the
# invoker deal with logging it or not
output="2>&1"

# Some other possible output postfixes:
#output="2>&1 | tee -a /var/tmp/bak-foo.log"
#output="2>&1 > /var/log/bak-foo.log"


prefix="rsync $commonSwitches"


date

set -x

# Trailing / on src dir means DON'T include the last dir from src
$prefix /boot $destPath $output
$prefix /etc $destPath $output
$prefix --exclude .mozilla/firefox/*/Cache --delete-excluded /home $destPath $output
$prefix /root $destPath $output
$prefix /var/lib/dpkg $destPath $output
$prefix /var/log $destPath $output
$prefix /var/mail $destPath $output
$prefix /var/spool/cron $destPath $output

set +x

date


# Installation:
#
#- Place a copy of this script somewhere important like
#  /etc/bak/bak-nightly.sh
#
#- Modify the sources and switch info in the backup commands, destPath,
#  etc data to reflect your backup needs
#  DON'T FORGET to turn off the --dry-run switch above once you think
#  it's ready!
#
#- Put something like this in your root user's crontab:
#   30 02 * * * /etc/bak/bak-nightly.sh > /var/log/bak-nightly.log
#
#- Perhaps get logrotate involved to keep the log from getting out of
#  control. A file like /etc/logrotate.d/bak containing:
#
#   /var/log/bak-nightly.log {
#      rotate 7
#      daily
#      compress
#      delaycompress
#      missingok
#      notifempty
#   }
