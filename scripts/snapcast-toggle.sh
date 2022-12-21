#!/bin/bash

# A helper script to move all sink inputs to Snapcast.
if [ -z "$1" ]; then
	echo "Usage: $0 (on | off)" >&2
	exit 1
fi

snapSink=$(pactl list short sinks | grep Snapcast | cut -f 1)
alsaSink=$(pactl list short sinks | grep alsa | cut -f 1)

if [ -z "$snapSink" ]; then
	echo "PulseAudio sink with name Snapcast not found"
	exit 1
fi

[[ $1 = "on" ]] && targetSink=$snapSink || targetSink=$alsaSink
echo "Moving all sink-inputs to sink: '$targetSink'"

pactl list short sink-inputs | cut -f 1 | while read id; do
	pactl move-sink-input "$id" "$targetSink"
done

client=$(pgrep snapclient)
if [[ -z "$client"  && "$1" = "on" ]]; then
	echo "No instance of snapclient running locally. Starting on localhost"
	snapclient -h 0.0.0.0 --player pulse > /dev/null 2>&1 &
fi

if [[ $1 = "off" ]]; then
	killall snapclient
fi
