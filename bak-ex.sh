#! /bin/sh

commonSwitches="-av -R --delete"
prefix="rsync $commonSwitches"
dest="user@host.blah:/top/level/remote/backup/dir"
output="2>&1"

date

# Trailing / on src dir means DON'T include the last dir from src
$prefix /var/spool/cron $dest $output
$prefix /var/mail $dest $output
$prefix /var/log $dest $output
$prefix /root $dest $output
$prefix --exclude .mozilla/firefox/*/Cache --delete-excluded /home $dest $output
$prefix /etc $dest $output
$prefix /boot $dest $output

date
