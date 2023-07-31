#!/bin/bash

file="$HOME/bookmark"

readarray -t bookmark < "$file"
selected_alias=$(printf '%s\n' "${bookmark[@]}" | dmenu -i -l 20 -p 'Open:' | awk '{print $1}')

if [ -n "$selected_alias" ]; then
    selected_link=$(printf '%s\n' "${bookmark[@]}" | grep "^$selected_alias" | cut -d " " -f 2-)
    firefox "$selected_link"
fi

