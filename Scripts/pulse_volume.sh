ACTION=$1
DEVICE=1
msgId="991049"

if [ $ACTION == increase ]; then
   if [ $(pamixer --get-volume-human) != "100%" ]; then
      pamixer -i 5
   fi
   dunstify -a "changeVolume" -u low -r "$msgId" "Vol: "$(pamixer --get-volume-human) 
elif [ $ACTION == decrease ]; then
   pamixer -d 5
   dunstify -a "changeVolume" -u low -r "$msgId" "Vol: "$(pamixer --get-volume-human) 
elif [ $ACTION == mute ]; then
   pamixer -t 
   dunstify -a "changeVolume" -u low -r "$msgId" "Vol: "$(pamixer --get-volume-human) 
else
   echo "Wrong action"
fi
