export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

export QT_QPA_PLATFORMTHEME=qt5ct

eval "$(dbus-launch --sh-syntax --exit-with-session)"
fcitx
setxkbmap -option altwin:ctrl_alt_win -option caps:menu&
#xrandr --output LVDS-1 --off --output VGA-1 --auto &
xsetroot -cursor_name X_cursor
xbacklight -set 50
exec stumpwm
