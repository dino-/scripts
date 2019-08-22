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

  $ clamwrap.sh -ri

UPDATING THE VIRUS DATABASE

If you get a warning from clamav about old virus defs, do this:

    # freshclam

INFECTION FALSE-POSITIVES

If you get reports of a file or files infected with Unix.Trojan.Mirai, it may
be a false-positive with the local clamav. Try checking the files with an
online scanner like https://www.virustotal.com

v1.2  2019-08-22  Dino Morelli <dino@ui3.info>

USAGE
}


case "$1" in
  -h|--help) usage; exit 0;;
esac

set -x
clamscan --bytecode-timeout=300000 --max-filesize=4000M --max-scansize=4000M "$@"
