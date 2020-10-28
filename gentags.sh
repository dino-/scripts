#! /bin/sh

# This is older, doesn't seem to work as well as letting hasktags recurse
# itself
# find src -regex '.*\..?hs' | xargs hasktags -c

# 2020-10-05 Recent versions of vim are less forgiving of improperly-formatted
# ctags lines. For apparently a very long time, hasktags has had bugs where
# some lines are created with a (weird) first field and nothing else. No file
# or line numbers.
# The command below post-processes the output to remove these lines and writes
# to the default filename, tags. The character we're matching in the [ ] is
# ctrl-I, a tab.
hasktags -c . -o - | egrep ".*[	].*" - > tags
