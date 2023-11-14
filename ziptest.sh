#! /usr/bin/env bash

unzip -tq $1 2>/dev/null > /dev/null || echo $1
#echo "$? $1"
