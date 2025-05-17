autoload -U colors && colors
PS1="%{$fg[magenta]%}%~%b λ "

export PF_ASCII="arch linux"

# History in cache directory: HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

#python 
alias psource='source prjenv/bin/activate'

#nvim
alias v='nvim'

alias ydp='yt-dlp -i -f best -o "%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" --extract-audio --audio-format mp3 --embed-metadata --parse-metadata "artist:%(uploader)s" --parse-metadata "album:%(playlist_title)s"'
alias yds='yt-dlp -i -f best -o "%(title)s.%(ext)s" --extract-audio --audio-format mp3 --embed-metadata --parse-metadata "artist:%(uploader)s" --parse-metadata "album:%(title)s"'

#git 
alias config='/usr/bin/git --git-dir=$HOME/Dotfiles.git/ --work-tree=$HOME'
alias gitg='git log --oneline --decorate --graph --all --parents'

#redshift
alias red='redshift -O 4100'

#cat
alias dog='bat'

#short cut
alias bashrc='nvim ~/.bashrc'
alias compose='setxkbmap -layout us -option compose:menu'
alias sshfix='TERM=linux'
alias cclear='clear; colorscript random | tail -n +2'
alias search="sudo find / -iname "
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'
alias parsua='paru -Sua --noconfirm' 

#ls (eza)
alias ll='eza --icons -al --color=always --group-directories-first' # my preferred listing
alias la='eza --icons -a --color=always --group-directories-first'  # all files and dirs
alias ls='eza --icons -l --color=always --group-directories-first '  # long format
alias lt='eza --icons -aT --color=always --group-directories-first -L 2' # tree listing
alias ltf='eza --icons -aT --color=always --group-directories-first -L 100'


#pacman - paru
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'  # remove orphaned packagesi
alias paruUp='paru -Sua --noconfirm'             # update only AUR pkgs (paru)

#grep
alias grep='grep --color=auto'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'

#info
alias free='free -m' # show sizes in MB
alias crypto='curl -s rate.sx'
alias price="curl -s rate.sx/doge |awk 'NR==34'"
alias doge="curl -s rate.sx/doge"
alias btc="curl -s rate.sx/btc"
alias cpu="ps ax -o cmd,%cpu --sort=-%cpu | head "
alias ram="ps ax -o cmd,%mem --sort=-%mem | head "
alias gpu="nvidia-smi | sed -n '10p' | boxes -d stone -p a2v1 | lolcat -f"
alias status="cpu;gpu;ram"
alias rice="curl -L rum.sh/ricebowl"
alias moon="curl wttr.in/moon"
alias weather="curl --silent wttr.in | head -n 6"
alias coffee="curl -L git.io/coffee "

#open
zff() {
  devour zathura "$(fzf)"
}

zffs() {
  devour zathura "$(find . -type f -iname '*.pdf' | grep -i "$1" | fzf)"
}

source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.config/zsh/zsh-vi-mode/zsh-vi-mode.plugin.zsh
source ~/.config/zsh/zap-prompt/myzap-prompt.zsh-theme 

pfetch
