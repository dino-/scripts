#! /usr/bin/env bash

binaryPath="${1:?Must supply a binary path to run}"

systemd-run --user --scope \
  -p MemoryMax=10G \
  -p CPUQuota=30% \
  "$binaryPath"
