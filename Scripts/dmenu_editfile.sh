EDITOR="nvim"
TERMINAL="alacritty -e"

file="$HOME/bookmarkfile"
readarray -t bookmarkfile< "$file"

item=$(printf '%s\n' "${bookmarkfile[@]}" |dmenu -i -l 20 -p 'Edit:')

if [[ -n "$item" ]]; then
    $TERMINAL $EDITOR "$item"
fi
