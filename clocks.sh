#! /bin/bash

# For a list of files for TZ, see /usr/share/zoneinfo

# shellcheck disable=SC2034
colRedNrm="\e[0;31m"  # Difficult to see
colRedBld="\e[1;31m"
colGreenNrm="\e[0;32m"
colYellowNrm="\e[0;33m"
colBlueNrm="\e[0;34m"  # Difficult to see
colBlueBld="\e[1;34m"  # Difficult to see
colPurpleNrm="\e[0;35m"  # Difficult to see
colPurpleBld="\e[1;35m"
colCyanNrm="\e[0;36m"
colWhiteNrm="\e[0;37m"

colReset="\e[0m"


function withColor() {
  color=$1
  zone=$2
  # timeFormat="%F %T %Z"
  timeFormat="%F %T %_4Z %z"

  echo -en "$color"; TZ="$zone" date +"$timeFormat $zone"; echo -en "$colReset"
}


while true
do
  clear

  withColor "$colYellowNrm" 'America/Los_Angeles'   # PST/PDT
  withColor "$colBlueBld" 'America/Denver'          # MT
  withColor "$colCyanNrm" 'America/New_York'        # EST/EDT
  withColor "$colGreenNrm" 'UTC'
  withColor "$colPurpleBld" 'Europe/Berlin'         # CET
  withColor "$colRedBld" 'EET'                      # Eastern European Time (EET)
  withColor "$colBlueBld" 'Asia/Kolkata'            # All of India (IST)
  withColor "$colCyanNrm" 'Portugal'                # WET

  sleep 1
done
