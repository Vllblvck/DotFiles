#!/bin/sh

if ! updates=$(yay -Qu --aur 2> /dev/null | wc -l); then
    updates=0
fi

echo "Aur: $updates"
