export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export XMODIFIERS=@im=fcitx5

export QT_QPA_PLATFORMTHEME=qt5ct

eval "$(dbus-launch --sh-syntax --exit-with-session)"
fcitx5&

xsetroot -cursor_name X_cursor
exec stumpwm
