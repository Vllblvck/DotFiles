#!/bin/sh

if ! updates=$(pacman -Qu 2> /dev/null | wc -l ); then
    updates=0
fi

echo "Arch : $updates"
