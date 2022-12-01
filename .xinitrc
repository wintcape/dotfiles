#!/bin/bash

## Initialize X (dpi, cursor, etc.)
xsetroot -cursor_name left_ptr
xrandr --dpi 192
xrandr --output HDMI-0 --mode 1920x1080 --pos 0x0 --scale 1.5x1.5
xrandr --output HDMI-1 --mode 3840x2160 --pos 3840x0 

## Initialize GDK
export GDK_SCALE=2

## Launch login manager
gnome-keyring-daemon --start
export SSH_AUTH_SOCK

# Launch audio server
pulseaudio --start


## CONTINUE: Launch window manager
exec dbus-launch --exit-with-session $HOME/.local/bin/xmonad
