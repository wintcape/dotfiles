#!/bin/sh

################################################################################

SRC_DIR=$HOME/.config/xmonad

################################################################################

FAIL=0
xmonad --recompile 2>$SRC_DIR/.log || FAIL=1
if [ $FAIL == 1 ]
then
    nvim $SRC_DIR/.log
else
    xmonad --restart
fi
