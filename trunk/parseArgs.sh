#! /bin/sh

# This script contains generic arg parsing code using the builtin getopts.
#
# Try a sample command line like:
#   parseArgs -cz -b foo bar baz


while getopts "ab:c" OPT
do
	# If it's an unwanted switch, skip rest of loop
	if [ "$OPT" == "?" ]
		then continue
	fi

	# Assign the value of the switch to a variable: opt_<switch>
	# Binary switches will be 1 for true and null for false
	if [ -z "$OPTARG" ]
		then eval opt_${OPT}=1
		else eval opt_${OPT}="$OPTARG"
	fi
done

# Shift past the args that were handled above, leaving the rest
shift $(($OPTIND - 1))

echo "opt_a: $opt_a"
echo "opt_b: $opt_b"
echo "opt_c: $opt_c"
echo "remaining: $*"
