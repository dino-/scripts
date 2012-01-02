#!/bin/sh

# This script was written to be called by xmobar

dir="/sys/devices/platform/coretemp.0"

core0=$(cat $dir/temp2_input)
core1=$(cat $dir/temp3_input)

echo "TempC: $(expr $core0 / 1000) $(expr $core1 / 1000)"
