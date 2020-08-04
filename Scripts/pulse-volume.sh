#!/bin/bash

if [[ $# -eq 0 ]]; then
    echo "No action specified"
    exit 0
fi

msgId="991049"

case "$1" in
    increase)
        if [ $(pamixer --get-volume-human) != "100%" ]; then
            pamixer -i 5
        fi
        dunstify -a "changeVolume" -u low -r "$msgId" " Vol: "$(pamixer --get-volume-human) 
        ;;
    decrease)
        pamixer -d 5
        dunstify -a "changeVolume" -u low -r "$msgId" "  Vol: "$(pamixer --get-volume-human) 
        ;;
    mute)
        pamixer -t 
        dunstify -a "changeVolume" -u low -r "$msgId" " Vol: "$(pamixer --get-volume-human) 
        ;;
    *)
        echo "Wrong action"
        ;;
esac
