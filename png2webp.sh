#! /bin/bash

inputImage="${1:?No file specified}"

outputImage=$(basename "$inputImage" .png).webp

convert "$inputImage" -define webp:lossless=true "$outputImage"
