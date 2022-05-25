#! /bin/bash

# For a list of files for TZ, see /usr/share/zoneinfo

# shellcheck disable=SC2034
colRedNrm="\e[0;31m"  # Difficult to see
colRedBld="\e[1;31m"
colGreenNrm="\e[0;32m"
colYellowNrm="\e[0;33m"
colYellowBld="\e[1;33m"
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
  comment=$3
  # timeFormat="%F %T %Z"
  timeFormat="%F %T %_4Z %z"

  echo -en "$color"
  TZ="$zone" date +"$timeFormat $zone - $comment"
  echo -en "$colReset"
}


while true
do
  clear

  withColor "$colYellowNrm" 'America/Los_Angeles' 'PST/PDT'
  withColor "$colBlueBld" 'America/Denver' 'MT'
  withColor "$colWhiteNrm" 'America/New_York' 'Dino  EST/EDT'
  withColor "$colGreenNrm" 'UTC'
  withColor "$colCyanNrm" 'Portugal' 'WET'
  withColor "$colPurpleBld" 'Europe/Berlin' 'CET'
  withColor "$colRedBld" 'EET' 'Eastern European Time'
  withColor "$colYellowBld" 'Asia/Kolkata' 'All of India (IST)'

  sleep 1
done
