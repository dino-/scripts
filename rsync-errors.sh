#! /bin/bash


basename=$(basename "$0")


function usage {
  cat <<USAGE
$basename - grep errors and warnings from an rsync log

usage:
  cat somelogfile | $basename [OPTIONS]

options:
  -h, --help  This help information
      --test  Run self-test

This script is looking for lines like these:

  'rsync: some message from rsync'
  'rsync error: some message from rsync'
  'ERROR some error message from rsync'
  'file has vanished ...'
  any line with '--dry-run' in it

Any other options will be passed to grep. For example:

    $ cat somelogfile | $0 -n -A1

If your log is coming from journald, as with our backups, usage may look like
this:

    $ journalctl -u bak@HOSTNAME --since today | $0

No output means no rsync warning messages were found in the log.

v2.0  2019-09-15  Dino Morelli <dino@ui3.info>

USAGE
}


function runtest {

  # This represents log data with lines we're interested in seeing,
  # one line for each.
  testOutput=$(cat <<TESTDATA | "$0"
2019-08-31 03:00:02> Executing command: rsync --dry-run blah blah blah
rsync: some message from rsync
rsync error: some message from rsync
ERROR some error message
file has vanished foo bar
Sep 10 03:00:46 machinename bak-machinename.sh[4134]: Executing command: rsync --dry-run blah blah blah
Sep 10 03:02:38 machinename bak-machinename.sh[4134]: rsync: some message from rsync
Sep 10 03:02:38 machinename bak-machinename.sh[4134]: rsync error: some message from rsync
Sep 10 03:02:39 machinename bak-machinename.sh[4134]: ERROR some error message
Sep 10 03:02:44 machinename bak-machinename.sh[4134]: file has vanished foo bar
TESTDATA
  )

  targetErrorLineCount=$(echo "$testOutput" | wc -l)

  errorLineCount=$(echo "$testOutput" | wc -l)
  [[ $errorLineCount -eq $targetErrorLineCount ]] || {
    echo "Test failed because we got $errorLineCount bad lines but expected $targetErrorLineCount"
    echo "Test output:"
    echo
    echo "$testOutput"
    exit 1
  }

}


# arg parsing

[ "$1" == "-h" ] && { usage; exit 0; }
[ "$1" == "--help" ] && { usage; exit 0; }
[ "$1" == "--test" ] && { runtest; exit 0; }

grep -E '(rsync: |rsync error: |(^| )[A-Z]{2,} |file has vanished|--dry-run)' "$@" -
