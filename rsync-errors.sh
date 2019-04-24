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

egrep '(^rsync: |^[A-Z]|--dry-run)' $@