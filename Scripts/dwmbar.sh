while :; do
    ram="ram $(free | grep "Mem:" | awk '{print $3 }')"
    cpu="cpu $(top -bn 1 | grep "Cpu(s)" | awk '{print 100-$8 "%"}')"
    date="$(date +%H:%M )" 
    weather="$(curl -s https://wttr.in/Palermo?0QT | awk ' NR ==1 {print $NF} NR==2 {print $(NF-1) $NF }'| tr '\n' ' ')"
    bar="| $weather | $date" 
    xsetroot -name "$bar"
    sleep 1m
done 
