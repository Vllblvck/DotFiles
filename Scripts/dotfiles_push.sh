commitMessage=$1
dotfiles="/usr/bin/git --git-dir=$HOME/Projects/DotFiles/ --work-tree=$HOME"

$dotfiles add .xmonad/ .xprofile .zshrc .xmobarrc .gtkrc-2.0 Pictures/wallpapers/ .moc .config/alacritty .config/gtk-3.0 .config/nvim .config/qutebrowser .config/dunstrc .config/picom.conf
$dotfiles commit -m "$commitMessage"
$dotfiles push origin master
