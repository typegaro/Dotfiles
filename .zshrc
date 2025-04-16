autoload -U colors && colors
PS1="%{$fg[magenta]%}%~%b λ "

export PF_ASCII="arch linux"
export XMODIFIERS= emacs

#JP
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
export CLUTTER_IM_MODULE=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=fcitx

### EXPORT
export PATH="$HOME/Scripts:$PATH"
export PATH=${PATH}:/usr/lib/jvm/default/bin
export PATH=$PATH:~/Scripts/
export PATH=$PATH:~/Android/Sdk/tools/bin/
export PATH=$PATH:~/Android/Sdk/tools/
export PATH=$PATH:~/Android/Sdk/emulator
export EXA_ICON_SPACING="2"
export CHROME_EXECUTABLE='/usr/bin/chromium'
export ANDROID_HOME=~/Android/Sdk
export ANDROID_HOME='/home/ldg/Android/Sdk'
export ANDROID_SDK_ROOT='/opt/android-sdk'

# History in cache directory: HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

#python 
alias psource='source prjenv/bin/activate'

#golang
export PATH=$PATH:~/go/bin

#latex
alias latexclean='rm *.aux *.log'

#speedreader
alias clip-speedread='xclip -o | speedread'

#nvim
alias v='nvim'
#Music
#alias ydp='yt-dlp -i -f best -o "%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" --extract-audio --audio-format mp3'
#alias yds='yt-dlp -i -f best -o "%(title)s.%(ext)s" --extract-audio --audio-format mp3'

alias ydp='yt-dlp -i -f best -o "%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" --extract-audio --audio-format mp3 --embed-metadata --parse-metadata "artist:%(uploader)s" --parse-metadata "album:%(playlist_title)s"'
alias yds='yt-dlp -i -f best -o "%(title)s.%(ext)s" --extract-audio --audio-format mp3 --embed-metadata --parse-metadata "artist:%(uploader)s" --parse-metadata "album:%(title)s"'

#git 
alias config='/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME'

# JP fcitx
export GTK_IM_MODULE='fcitx'
export QT_IM_MODULE='fcitx'
export SDL_IM_MODULE='fcitx'
export XMODIFIERS='@im=fcitx'

#andoird
alias phone='~/Android/Sdk/tools/emulator -avd Pixel_3a_API_33_x86_64'
alias phone-list='emulator -list-avds'

# utility 
alias src=' ls | grep'


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
#alias doc='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

#nnn
alias nnn='nnn -e'

#doom emacs alias
alias doom='~/.emacs.d/bin/doom'

#rec
alias reca='ffmpeg -f alsa -i hw:3,0  out.mp3'
alias recScreen2='ffmpeg -f x11grab -s 1920x1080 -i :1+1920 out.mkv'
alias recScreen1='ffmpeg -f x11grab -s 1920x1080 -i :1 out.mkv'

#trans
alias toen='trans -b :en' # translates from English to Italian
alias toit='trans -b :it' # translates from Italian to English

#ls (exa)
alias ll='exa --icons -al --color=always --group-directories-first' # my preferred listing
alias la='exa --icons -a --color=always --group-directories-first'  # all files and dirs
alias ls='exa --icons -l --color=always --group-directories-first '  # long format
alias lt='exa --icons -aT --color=always --group-directories-first -L 2' # tree listing
alias ltf='exa --icons -aT --color=always --group-directories-first -L 100'


#pacman - paru
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'  # remove orphaned packagesi
alias paruUp='paru -Sua --noconfirm'             # update only AUR pkgs (paru)

#grep
alias grep='grep --color=auto'

#keyboard
alias kit="sudo setxkbmap -layout it"
alias kus="sudo setxkbmap -layout us"

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'

#ffmpeg
alias ffmerge="ffmpeg -f concat -i mylist.txt -c copy output.mp4"
alias recgame="ffmpeg -f x11grab -video_size 1920x1080 -i :0.0 -an output.mp4"


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




# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH
# Baregit
alias config='/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME'
alias gitgodot='/usr/bin/git --git-dir=$HOME/Work/GodotComponents/ '



#source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.config/zsh/zap-prompt.zsh-theme

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

pfetch

# bun completions
[ -s "/home/ldg/.bun/_bun" ] && source "/home/ldg/.bun/_bun"
