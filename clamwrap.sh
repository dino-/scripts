#! /bin/bash

# A script to fill in some good defaults for clamscan that are often needed


basename=$(basename "$0")


function usage {
  cat <<USAGE
$basename - Wrapper script to add some useful defaults to clamscan

usage:
  $basename [OPTIONS]
  $basename [CLAMSCAN-OPTIONS] [DIR1] [DIR2] [FILE1] ...

options:
  -h, --help       This help information

frequently-used clamscan options:
  -r, --recursive  Scan subdirectories recursively
  -i, --infected   Only print infected files

No directory or file implies .

For more clamscan switches: $ clamscan --help

Most common usage:

  $ clamwrap.sh -r

v1.0  2018-08-27  Dino Morelli <dino@ui3.info>

USAGE
}


if [ "$1" == "-h" ]; then usage; exit 0; fi
if [ "$1" == "--help" ]; then usage; exit 0; fi

set -x
clamscan --bytecode-timeout=300000 --max-filesize=4000M --max-scansize=4000M $*
