##############################################################################
# ENVIROMENT VARIABLES
##############################################################################

export PATH="${PATH}:/home/vllblvck/.dotnet/tools"
export VISUAL="nvim"
export EDITOR="nvim"
export TERM="alacritty"

##############################################################################
# ALIASES 
##############################################################################

# General
alias yas="yay -S"
alias yasu="yay -Syu"
alias yar="yay -Rsn"
alias yaq="yay -Q"
alias yaqi="yay -Qi"
alias yac="yay -Yc"
alias l="ls --color"
alias la="ls --color -A"
alias ll="ls --color -l"
alias lal="ls --color -Al"
alias ..="cd .."
alias ...="cd ../.."
alias ~="cd ~"
alias dir="dirs -v"
alias rm="rm -I"

# Programs
alias v="nvim"
alias r="ranger"

# Scripts
alias ex="$HOME/Scripts/extract.sh"
alias yt="$HOME/Scripts/yt-music.sh"
alias dotfiles-add="$HOME/Scripts/dotfiles-add.sh"
alias arch-updates="$HOME/Scripts/arch-updates.sh"
alias aur-updates="$HOME/Scripts/aur-updates.sh"
alias pulse-volume="$HOME/Scripts/pulse-volume.sh"
alias timer="$HOME/Scripts/timer.sh"

# Misc
alias dotfiles="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"
alias weather="curl wttr.in"
alias xmonconf="nvim $HOME/.xmonad/xmonad.hs"
alias xbarconf="nvim $HOME/.config/xmobar/xmobarrc"
alias nvimconf="nvim $HOME/.config/nvim/init.vim"
alias zshrc="nvim $HOME/.zshrc"
alias alaconf="nvim $HOME/.config/alacritty/alacritty.yml"
alias pyemail="python -m smtpd -n -c DebuggingServer localhost:8025"

##############################################################################
# ZSH SETTINGS
##############################################################################

#Autocompletion
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' rehash true

#Prompt
VIMODE=''
function zle-keymap-select {
  VIMODE="${${KEYMAP/vicmd/(normal)}/(main|viins)/}"
  zle reset-prompt
}
zle -N zle-keymap-select

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
PROMPT='%(?.%F{green}√.%F{red}X)%f%  %F{blue}%2~%f'\$vcs_info_msg_0_' %F{magenta}%(!.#.$)%f '
RPROMPT='%F{green}${VIMODE}%f'

zstyle ':vcs_info:git:*' formats '%F{yellow} (%b)%f'
zstyle ':vcs_info:*' enable git

#Key bindings
bindkey -v
typeset -g -A key

key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word

#History 
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#Dirstack
autoload -Uz add-zsh-hook

DIRSTACKFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/dirs"
if [[ -f "$DIRSTACKFILE" ]] && (( ${#dirstack} == 0 )); then
    dirstack=("${(@f)"$(< "$DIRSTACKFILE")"}")
    [[ -d "${dirstack[1]}" ]] && cd -- "${dirstack[1]}"
fi
chpwd_dirstack() {
    print -l -- "$PWD" "${(u)dirstack[@]}" > "$DIRSTACKFILE"
}
add-zsh-hook -Uz chpwd chpwd_dirstack

DIRSTACKSIZE='20'
setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_MINUS

#Rehash
trap 'rehash' USR1
