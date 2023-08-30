dir=$(find ~ -type d | fzf)
if [ -z "$dir" ]; then
    echo "Nessuna directory selezionata."
    exit 1
fi
tmux new-session -s "Work" -c "$dir" -d
tmux split-window -h -c "$dir" 
tmux send-keys -t 1 'nvim .' C-m 
tmux attach-session 
