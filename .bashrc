#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Colors
alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Default text editor
export EDITOR=nvim
export VISUAL=$EDITOR

# Set display
export DISPLAY=:0

# Set steam dir
export STEAM_DIR=/opt/steam
