#!/bin/sh
#xcompmgr -r7 -l-10 -t-8 -D7 &
##Esetroot /home/wipeover/kep/backgrounds/mushroom.png
#Esetroot /home/wipeover/kep/backgrounds/skullbg.png
#/home/wipeover/bin/awesome-status.py&
unclutter -idle 2 -notclass qemu-system-x86_64 &
xcompmgr -c&
synclient HorizTwoFingerScroll=1
#exec /usr/bin/awesome
#/home/wipeover/.awesome/keymap.sh&
#eval `ssh-agent`
setxkbmap -layout us -option "ctrl:swapcaps" -option "altwin:super_win"
setxkbmap -option grp:switch,grp:rctrl_switch,grp_led:scroll us,hu
feh --bg-scale --no-fehbg /home/a/i/bg/gray-robot-pattern.png
#xhost +si:localuser:b
#xcompmgr -c -f -F -o 0.5&
#wmii
#tritium
(sleep 4 && conky)&

urxvtd&
#exec xmonad
#openbox
#sawfish
#export PYTHONPATH="/home/wipeover/misc/whismy:$PYTHONPATH"
#urxvt
#xcompmgr -c &
#stumpwm
#sbcl --load /home/wipeover/misc/clfswm-0906/load.lisp
#while true; do sleep 1000000; done
#python2.6 /home/wipeover/misc/whimsy/config.py
#~/qtile/qtile # XCBSurface
#pekwm
#(sleep 4; conky -c /home/wipeover/.config/wmfs/conkyrc | while read -r; do wmfs -s -name "$REPLY"; done)&
#/home/wipeover/bin/wmfs
#echo "Starting at $(date)" >> ~/.i3/logfile
#(sleep 2 && i3bar_old -m --color-urgent-ws-bg "FFFF00" --color-urgent-ws-fg "000000")&
#exec /home/wipeover/bin/i3 -V -d all >> ~/.i3/logfile
#python /usr/share/doc/python-plwm/examples/hrwwm.py
#exec herbstluftwm
exec i3

#exec stumpwm
