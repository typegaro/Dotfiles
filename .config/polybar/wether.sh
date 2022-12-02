#!/bin/sh


time=$(date '+%H')
if ['$time' -lt '12'];then
    morning_rain=$(curl wttr.in/Palermo | sed -n 16,5p |awk '{print $11}')
    echo "$morning_rain"
fi

#noon_rain=$(curl wttr.in/Palermo | sed -n 16,5p |awk '{print $22}') 

#evening_rain=$(curl wttr.in/Palermo | sed -n 16,5p |awk '{print $33}')

#night_rain=$(curl wttr.in/Palermo | sed -n 16,5p |awk '{print $44}')
