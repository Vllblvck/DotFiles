# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Enviroment variables
export ZSH="/home/vllblvck/.oh-my-zsh"
export PATH="${PATH}:/home/vllblvck/.dotnet/tools"
export VISUAL="/usr/bin/nvim"
export EDITOR="/usr/bin/nvim"
export TERM="alacritty"

# Plugins
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=( 
 extract
 chucknorris
)

source $ZSH/oh-my-zsh.sh

# Aliases
alias yas="yay -Syu"
alias yar="yay -Rsn"
alias yaq="yay -Qi"
alias ex="extract"
alias r="ranger"
alias v="nvim"
alias ..="cd .."
alias yt="~/Scripts/ytmusic.sh"
alias weather="curl wttr.in"
alias dotfiles="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"
alias dotfiles-push="./Scripts/dotfiles-push"
alias xmonconf="nvim ~/.xmonad/xmonad.hs"
alias xbarconf="nvim ~/.config/xmobar/xmobarrc"
alias l="ls"
alias la="ls -A"
alias ll="ls -l"
alias lal="ls -Al"

# Vi mode
# bindkey -v

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
