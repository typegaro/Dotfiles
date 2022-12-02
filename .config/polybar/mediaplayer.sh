#!/bin/sh
player_status=$(playerctl -p spotify status 2> /dev/null)
if [ "$player_status" = "Playing" ]; then
    echo " $(playerctl -p spotify metadata title)"
elif [ "$player_status" = "Paused" ]; then
    echo " "
elif [ "$player_status" = "No players found" ]; then
    echo " "
fi
