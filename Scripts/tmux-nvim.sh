dir=$(find ~ -type d | fzf)
if [ -z "$dir" ]; then
    exit 1
fi
tmux new-session -s "Work" -c "$dir" -d
tmux new-window -c "$dir" 
tmux send-keys -t 1 'nvim .' C-m 
tmux attach-session 
