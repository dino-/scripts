#!/bin/sh

# This script was written to be called by xmobar

dir="/sys/devices/platform/coretemp.0"

core0=$(cat $dir/temp2_input)
core1=$(cat $dir/temp3_input)
core2=$(cat $dir/temp4_input)
core3=$(cat $dir/temp5_input)

echo "$(expr $core0 / 1000)C $(expr $core1 / 1000)C $(expr $core2 / 1000)C $(expr $core3 / 1000)C"
