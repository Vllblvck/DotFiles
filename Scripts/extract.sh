#!/bin/bash

if [[ $# -eq 0 ]]; then
    echo "No file specified"
    exit 0
fi

case "$1" in
    *.tar.bz2)   tar xjf "$1"    ;;
    *.tar.gz)    tar xzf "$1"    ;;
    *.bz2)       bunzip2 -k "$1" ;;
    *.rar)       unrar e "$1"    ;;
    *.gz)        gunzip -k "$1"  ;;
    *.tar)       tar xf "$1"     ;;
    *.tbz2)      tar xjf "$1"    ;;
    *.tgz)       tar xzf "$1"    ;;
    *.zip)       unzip "$1"      ;;
    *.Z)         uncompress "$1" ;;
    *.7z)        7z x "$1"       ;;
    *)           echo "'$1' can't be extracted by this script" ;;
esac
