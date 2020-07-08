#!/bin/bash

if [[ $# -eq 0 ]]; then
    echo "No commit message"
    exit 0
fi

dotfiles=(
	"Scripts/"
	"Pictures/wallpapers/"
	".moc/"
	".xmonad/"
    ".gtkrc-2.0"
    ".p10k.zsh"
    ".xprofile"
    ".zshrc"
    ".config/alacritty/"
    ".config/calcurse/"
    ".config/cheat/"
    ".config/dunstrc"
    ".config/gtk-3.0/"
    ".config/nvim/"
    ".config/picom.conf"
    ".config/qutebrowser/"
    ".config/ranger"
    ".config/xmobar/"
)

git="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"

for file in "${dotfiles[@]}"; do
    $git add "$file"
done

$git commit -m "$1"
$git push origin master
