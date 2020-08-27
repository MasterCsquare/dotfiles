export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export XMODIFIERS=@im=fcitx5

export QT_QPA_PLATFORMTHEME=qt5ct

fcitx5&

xrandr --output LVDS-1 --off --output VGA-1 --auto &
exec stumpwm
