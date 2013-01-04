#! /bin/bash

# Printer-specific options
# These BR* options below are for my Brother printer, to duplex and
# use color. Another useful one may be: -P <YourPrinterIP> if you
# don't have a default printer set.

lprOptions="-o BRDuplex=DuplexNoTumble -o BRMonoColor=Color"


usage() {
   scriptName=$(basename $0)

   echo $1
   echo
   echo "$scriptName - Print course slides 6-up"
   echo
   echo "usage:"
   echo "   $scriptName SLIDES_PDF_FILE"
   echo
   echo "This script requires that these programs are installed:"
   echo "   - psf2ps"
   echo "   - psnup"
   echo "   - lpr"

   exit 1
}


pdfFile=$1
[ -f "$pdfFile" ] || usage "No PDF file given to print"


# Echo commands from here on out
set -x

# Prepare and print the document
pdf2ps $pdfFile - | \
psnup -r -w8.5in -h11in -W6.3in -H3.54in -3 -b0.5cm | \
lpr $lprOptions
