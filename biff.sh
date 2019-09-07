#! /bin/bash

# A low-tech biff program for times when X isn't readily available

maildir="$HOME/.mail"

# All mail folders except Trash
allMailFolders=$(find "$maildir" -not -regex '.*Trash.*' -name new | sort)

for dir in $allMailFolders
do
  unreadCount=$(find "$dir" -type f | wc -l)

  # Show only folders that have unread mail
  [[ $unreadCount -gt 0 ]] && {
    # shellcheck disable=SC2046,SC2086
    folderName="$(splitpath --takebasename $(splitpath --takedirectory $dir))"
    printf "%4d %s\n" "$unreadCount" "$folderName"
  }
done
