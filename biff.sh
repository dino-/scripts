#! /bin/bash

# A low-tech biff program for times when X isn't readily available

maildir="$HOME/.mail"

# All mail folders with some exceptions
allMailFolders=$(find "$maildir" \( -not -regex '.*Trash.*' -and -not -regex '.*-learn.*' \) -name new | sort)

for dir in $allMailFolders
do
  unreadCount=$(find "$dir" -type f | wc -l)

  # Show only folders that have unread mail
  [[ $unreadCount -gt 0 ]] && {
    folderName=$(splitpath --takedirectory "$dir" | xargs splitpath --takebasename)
    printf "%4d %s\n" "$unreadCount" "$folderName"
  }
done
