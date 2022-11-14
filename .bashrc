#██████╗  ██████╗ ███╗   ███╗██████╗ ██╗ ██████╗ █████╗ ██████╗  ██████╗
#██╔══██╗██╔═══██╗████╗ ████║██╔══██╗██║██╔════╝██╔══██╗██╔══██╗██╔═══██╗
#██████╔╝██║   ██║██╔████╔██║██████╔╝██║██║     ███████║██████╔╝██║   ██║
#██╔══██╗██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══██║██╔═══╝ ██║   ██║
#██║  ██║╚██████╔╝██║ ╚═╝ ██║██║     ██║╚██████╗██║  ██║██║     ╚██████╔╝
#╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝ ╚═════╝╚═╝  ╚═╝╚═╝      ╚═════╝
#Config by Rompicapo

colorscript random  
# If not running interactively, don't do anything
[[ $- != *i* ]] && return



#PS1="$(prompt) →  ";
PS1='λ → '

### EXPORT
export EDITOR=vim
export VISUAL=vim
export TERM="xterm-256color"                      # getting proper colors
export HISTCONTROL=ignoredups:erasedups           # no duplicate entries
export PATH=$PATH:~/.local/bin/
export PATH=${PATH}:/usr/lib/jvm/default/bin
export PATH=$PATH:~/Scripts/
export PATH=$PATH:~/Android/Sdk/tools/bin/
export PATH=$PATH:~/Android/Sdk/tools/
export PATH=$PATH:~/Android/Sdk/emulator
export EXA_ICON_SPACING="2"
export CHROME_EXECUTABLE='/usr/bin/chromium'
export ANDROID_HOME=~/Android/Sdk
#hyperland 
export LIBVA_DRIVER_NAME=nvidia
export XDG_SESSION_TYPE=wayland
export GBM_BACKEND=nvidia-drm
export __GLX_VENDOR_LIBRARY_NAME=nvidia
export WLR_NO_HARDWARE_CURSORS=1


#nvim
alias v='nvim'
#git 
alias config='/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME'

# JP fcitx
export GTK_IM_MODULE='fcitx'
export QT_IM_MODULE='fcitx'
export SDL_IM_MODULE='fcitx'
export XMODIFIERS='@im=fcitx'

#andoird
alias phone='emulator -avd Pixel_3a_API_33_x86_64'
alias phone-list='emulator -list-avds'

# utility 
alias src=' ls | grep'

# swallowing
alias mpv='devour mpv'
alias zathura='devour zathura'
alias feh='devour feh'
alias libreoffice='devour libreoffice'
alias code='devour code'

#redshift
alias red='redshift -O 4100'
#cat
alias cat='bat'

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

#xmonad
alias xmonad-restart="xmonad --recompile;xmonad --restart"

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

### ARCHIVE EXTRACTION by DistroTube
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

eval "$(starship init bash)"
