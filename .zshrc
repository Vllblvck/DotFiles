##############################################################################
# ZSH SETTINGS
##############################################################################

#Autocompletion
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' rehash true

#Prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

#Key bindings
bindkey -v
typeset -g -A key

key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word

##############################################################################
# ENVIROMENT VARIABLES
##############################################################################

export PATH="${PATH}:/home/vllblvck/.dotnet/tools"
export VISUAL="/usr/bin/nvim"
export EDITOR="/usr/bin/nvim"
export TERM="alacritty"

##############################################################################
# ALIASES 
##############################################################################

# General
alias yas="yay -S"
alias yasu="yay -Syu"
alias yar="yay -Rsn"
alias yaq="yay -Qi"
alias yac="yay -Yc"
alias l="ls"
alias la="ls -A"
alias ll="ls -l"
alias lal="ls -Al"
alias ..="cd .."
alias ~="cd ~"

# Programs
alias v="nvim"
alias r="ranger"

# Scripts
alias ex="$HOME/Scripts/extract.sh"
alias yt="$HOME/Scripts/yt-music.sh"
alias dotfiles-push="$HOME/Scripts/dotfiles-push.sh"
alias arch-updates="$HOME/Scripts/arch-updates.sh"
alias aur-updates="$HOME/Scripts/aur-updates.sh"
alias pulse-volume="$HOME/Scripts/pulse-volume.sh"

# Misc
alias dotfiles="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"
alias weather="curl wttr.in"
alias xmonconf="nvim $HOME/.xmonad/xmonad.hs"
alias xbarconf="nvim $HOME/.config/xmobar/xmobarrc"
alias nvimconf="nvim $HOME/.config/nvim/init.vim"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
