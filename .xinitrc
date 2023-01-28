# -*- mode:posix-shell -*-
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export GLFW_IM_MODULE=ibus

#export QT_QPA_PLATFORMTHEME=qt5ct

eval "$(dbus-launch --sh-syntax --exit-with-session)"
ibus-daemon -d
setxkbmap -option caps:menu
#xrandr --output eDP --off --output HDMI-A-0 --primary --auto
xsetroot -cursor_name left_ptr
xbacklight -set 30
#pulseaudio -D
#feh --bg-scale ~/wallpaper.jpg
exec xmonad