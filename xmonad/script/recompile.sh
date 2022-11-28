#!/bin/sh

################################################################################

LOG=$HOME/.config/xmonad/.log

################################################################################
FAIL=0
xmonad --recompile 2>$LOG || FAIL=1
if [ $FAIL == 0 ]
then
    xmonad --restart
else
    nvim $LOG
fi
