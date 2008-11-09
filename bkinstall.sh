#! /bin/bash


mp="/media/prs505"
src="$HOME/temp/books/install"
dest="$mp/database/media/books"

mount $mp
echo "mounted reader at $mp"

cp -v $src/* $dest

rm -fv $src/*

umount $mp
echo "unmounted reader"
