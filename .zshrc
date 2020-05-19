# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to oh-my-zsh installation.
export ZSH="/home/vllblvck/.oh-my-zsh"

#Plugins
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=( 
 git
 colorize
 cp
 extract
 history 
 chucknorris
)

source $ZSH/oh-my-zsh.sh

#aliases
alias zshrc="vim /home/vllblvck/.zshrc"
alias r="//usr/bin/ranger"
alias notes="cd /home/vllblvck/Documents/ && vim mynotes.txt"
alias ls="colorls"
alias lsa="colorls -A"
alias lsl="colorls -l"
alias lsal="colorls -al"
alias ..="cd .."
alias bt="bashtop"
alias orphans="sudo pacman -Rsn $(pacman -Qqtd)"
alias yt="python3 ~/Projects/YtMusicDownloader/downloadytmusic.py"
alias dotfiles="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
