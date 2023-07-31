#!/bin/bash

file="$HOME/speedsearch"

readarray -t speedsearch < "$file"
selected_alias=$(printf '%s\n' "${speedsearch[@]}" | dmenu -i -l 20 -p 'Open:' | awk '{print $1}')

if [ -n "$selected_alias" ]; then
    selected_link=$(printf '%s\n' "${speedsearch[@]}" | grep "^$selected_alias" | cut -d " " -f 2-)
    key=$(printf '' | dmenu -i -l 20 -p 'Key:')
    key="${key// /+}"
    firefox "$selected_link$key"
fi

