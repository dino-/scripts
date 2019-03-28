#! /bin/bash

# For a list of files for TZ, see /usr/share/zoneinfo

txtreset="\e[0m"

txtred="\e[0;31m"  # Difficult to see
bldred="\e[1;31m"
txtgreen="\e[0;32m"
txtyellow="\e[0;33m"
txtblue="\e[0;34m"  # Difficult to see
bldblue="\e[1;34m"  # Difficult to see
txtpurple="\e[0;35m"  # Difficult to see
bldpurple="\e[1;35m"
txtcyan="\e[0;36m"
txtwhite="\e[0;37m"

timeFormat="%F %T %Z"

while true
do
   clear

   echo -en "$txtyellow"; TZ='America/Los_Angeles' date +"$timeFormat"; echo -en "$txtreset"
   echo -en "$txtcyan"; TZ='America/New_York' date +"$timeFormat"; echo -en "$txtreset"
   echo -en "$txtgreen"; TZ='UTC' date +"$timeFormat"; echo -en "$txtreset"
   # echo -en "$bldpurple"; TZ='EET' date +"$timeFormat"; echo -en "$txtreset"

   sleep 1
done
