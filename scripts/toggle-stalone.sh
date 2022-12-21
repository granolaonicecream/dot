
# Trayer starts/stops fast enough to not warrant figuring out how to hide it
# If trayer is running, kill all trayer instances.  Else, start trayer
running_process=$(pgrep stalonetray)
if [ ! -z $running_process ]
then
    kill $running_process
else
    stalonetray --geometry 1x1+2538+2 --grow-gravity E -bg "#351409" -i 17 --kludges force_icons_size &
fi
