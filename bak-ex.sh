#! /bin/sh

commonSwitches="-av -R --delete"
prefix="rsync $commonSwitches"
dest="user@host.blah:/top/level/remote/backup/dir"
postfix="2>&1"

date

# Trailing / on src dir means DON'T include the last dir from src
$prefix /var/spool/cron $dest $postfix
$prefix /var/mail $dest $postfix
$prefix /var/log $dest $postfix
$prefix /root $dest $postfix
$prefix --exclude .mozilla/firefox/*/Cache --delete-excluded /home $dest $postfix
$prefix /etc $dest $postfix
$prefix /boot $dest $postfix

date
