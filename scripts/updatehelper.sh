#!/bin/bash

if [ -z $DISPLAY ]; then
	echo "DISPLAY is not set. Skipping update check"
	exit 0
fi


# TODO: This script needs to be system level for the pacman hook to call it 
# without specifically targeting a user directory.  Means systemd can't be
# run as user to do the timer
CACHE_DIR="/home/archie/.cache"
CACHE_FILE="$CACHE_DIR/last-update-check.txt"

# 1. Call the pacman helper checkupdates and cache the result in a file.
# 2. Number of lines in the file should be number of pending updates.
# 3. With that number, set an xprop with a formatted string.
# An xprop on the root window is used only because xmobar lacks good messaging
# to plugins. However, it does have the ability to read an xprop as a log, so
# this is the most straightforward solution.
function check_and_set {
	checkupdates > $CACHE_FILE
	pending=$(wc -l < $CACHE_FILE)
	if [ -z $DISPLAY ]; then
		echo "DISPLAY is not set. Skipping xprop set"
	else
		xprop -root -format _UPDATES_PENDING 8u -set _UPDATES_PENDING "<fc=#d0902f,#351409:0>  $pending</fc>"
	fi
}

if [[ $1 == "check" ]]; then
	check_and_set
else
	updates=$(<$CACHE_FILE)
	pending=$(wc -l < $CACHE_FILE)
	if [[ ! $pending -eq "0" ]]; then
		dunstify -i none "Pending Updates: \
			\
			$updates" -h string:x-dunst-stack-tag:updates 
		# -h string:bgcolor:#351409 -h string:frcolor:#a15501 -h string:fgcolor:#d0902f
	else
		dunstify -i none "All packages are up to date" -h string:x-dunst-stack-tag:updates
	fi
fi

