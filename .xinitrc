export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export XMODIFIERS=@im=fcitx5

export QT_QPA_PLATFORMTHEME=qt5ct

eval "$(dbus-launch --sh-syntax --exit-with-session)"
fcitx5&
setxkbmap -option altwin:ctrl_alt_win -option caps:menu&
#xrandr --output eDP --off --output HDMI-A-0 --auto&
xsetroot -cursor_name X_cursor
xbacklight -set 40
exec stumpwm
