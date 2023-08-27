#! /usr/bin/env bash

# secure hd erase

# This method is VERY fast. Make absolutely sure of the device!

deviceToNuke=${1:?"ERROR! Usage: $0 DEVICE"}
passphrase=$(tr -cd '[:alnum:]' < /dev/urandom | head -c128)
openssl enc -aes-256-cbc -md sha512 -pbkdf2 -iter 100000 -pass pass:"$passphrase" -salt < /dev/zero | dd bs=64K ibs=64K of="$deviceToNuke" status=progress
unset passphrase
