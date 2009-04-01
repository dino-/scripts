#! /bin/bash


mp="/media/prs505/int"
src="$HOME/temp/books/install"
dest="$mp/database/media/books"

mount $mp
echo "mounted reader at $mp"

cp -v $* $dest

umount $mp
echo "unmounted reader"
