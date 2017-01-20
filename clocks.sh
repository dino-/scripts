#! /bin/bash

cyan="\e[36m"
green="\e[32m"
yellow="\e[33m"
endColor="\e[0m"


while true
do
   clear

   echo -en "$green"; date -u +"%F %T %Z"; echo -en "$endColor"
   echo -en "$yellow"; date +"%F %T %Z"; echo -en "$endColor"
   echo -en "$cyan"; TZ='America/Los_Angeles' date +"%F %T %Z"; echo -en "$endColor"

   sleep 1
done
