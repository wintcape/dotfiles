#
# ~/.bashrc
#


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Set display
export DISPLAY=:0

# Default text editor
export EDITOR=nvim
export VISUAL=$EDITOR

# Colors
alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
