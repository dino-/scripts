#! /bin/bash

# Script to grep errors and warnings from an rsync log
#
# Expects a filename like:
#   $ rsync-errors.sh /some/rsync/log/file
#
# Pass additional arguments like this:
#   $ rsync-errors.sh -n -A1 /some/rsync/log/file
#
# Matches lines like these:
#
#   'rsync: some message from rsync'
#   'ERROR some error message from rsync'
#   'any line with --dry-run in it'

# Isolate the last argument
logFile="${!#}"

if [ -s $logFile ]
  then
    egrep '(^rsync|^[A-Z]{2,}|^file has vanished|--dry-run)' $@  # Calling with all arguments
  else
    echo "ERROR: File $logFile is empty, may have been logrotated, try ${logFile}.1"
fi
