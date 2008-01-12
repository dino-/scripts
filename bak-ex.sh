#! /bin/sh

commonSwitches="-av -R --delete"
prefix="rsync $commonSwitches"
destPath="user@host.blah:/top/level/remote/backup/dir"
output="2>&1"

date

# Trailing / on src dir means DON'T include the last dir from src
$prefix /boot $destPath $output
$prefix /etc $destPath $output
$prefix --exclude .mozilla/firefox/*/Cache --delete-excluded /home $destPath $output
$prefix /root $destPath $output
$prefix /var/lib/dpkg $destPath $output
$prefix /var/log $destPath $output
$prefix /var/mail $destPath $output
$prefix /var/spool/cron $destPath $output

date
