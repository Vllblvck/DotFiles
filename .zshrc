# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Paths
export ZSH="/home/vllblvck/.oh-my-zsh"
export PATH="${PATH}:/home/vllblvck/.dotnet/tools"
export EDITOR="usr/bin/nvim"
export TERM="alacritty"

# Plugins
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=( 
 extract
 chucknorris
)

source $ZSH/oh-my-zsh.sh

# Aliases
alias ex="extract"
alias r="ranger"
alias v="nvim"
alias lsa="ls -A"
alias lsl="ls -l"
alias lsal="ls -Al"
alias ..="cd .."
alias bt="bashtop"
alias yt="sh ~/Projects/Scripts/ytmusic.sh"
alias dotfiles="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"

# Vi mode
bindkey -v

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
