# -*- mode:posix-shell -*-
export ZSH="/home/mas/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(zsh-autosuggestions zsh-syntax-highlighting)

zstyle ':omz:update' mode disabled

source $ZSH/oh-my-zsh.sh

alias s="sudo "
alias m="mpv"
alias x="startx"
alias kat="kitty +kitten icat"
alias kiff="kitty +kitten diff"
alias sc="sdcv -c"
alias poweroff="sudo openrc-shutdown -p now"
alias reboot="sudo openrc-shutdown -r now"