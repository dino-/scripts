#! /bin/sh

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
