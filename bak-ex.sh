#! /bin/sh

commonSwitches="-av -R --delete"
prefix="rsync $commonSwitches"
destPath="user@host.blah:/top/level/remote/backup/dir"
output="2>&1"

date

# Trailing / on src dir means DON'T include the last dir from src
$prefix /var/spool/cron $destPath $output
$prefix /var/mail $destPath $output
$prefix /var/log $destPath $output
$prefix /root $destPath $output
$prefix --exclude .mozilla/firefox/*/Cache --delete-excluded /home $destPath $output
$prefix /etc $destPath $output
$prefix /boot $destPath $output

date
