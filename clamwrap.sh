#! /bin/bash

# A script to fill in some good defaults for clamscan that are often needed

clamscan --bytecode-timeout=300000 --max-filesize=4000M --max-scansize=4000M $*
