#!/bin/bash

# Set env
export LC_ALL=C                             # locale
export PATH="${PATH}:${HOME}/.local/bin"    # path
export EDITOR=nvim                          # text editor
export VISUAL=$EDITOR
export DISPLAY=:0                           # display
export STEAM_DIR=/opt/steam                 # steam dir

# Init shell
#[[ -f ~/.bashrc ]] && . ~/.bashrc

# Start X server
startx
