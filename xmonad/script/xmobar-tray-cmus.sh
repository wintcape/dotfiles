#!/bin/bash

# Ensure cmus player is running, then fetch player info
if [[ ! $(pgrep cmus) ]]; then
    exit 0
fi
CMUS_DUMP=$(cmus-remote -Q)

# Get player status, exit if stopped
STATUS=$(printf '%s' "$CMUS_DUMP" | awk 'NR==1{print $2}')
if [[ $STATUS == 'stopped' ]]; then
    exit 0
fi
if [[ $STATUS = 'paused' ]]; then
    STATUS='||'
else
    if [[ $STATUS = 'playing' ]]; then
        STATUS='|>'
    fi
fi

# Get current artist - defaults to '?' if none
ARTIST=$(printf '%s' "$CMUS_DUMP" | grep 'tag artist ')
if [[ $ARTIST ]]; then
    ARTIST=$(printf '%s' "$ARTIST" | awk '{for (i=3; i<=NF; i++) printf "%s ",$i}')
else
    ARTIST='?'
fi

# Get current title - defaults to filename if none
TITLE=$(printf '%s' "$CMUS_DUMP" | grep 'tag title ')
if [[ $TITLE ]]; then
    TITLE=$(printf '%s' "$TITLE" | awk '{for (i=3; i<=NF; i++) printf "%s ",$i}')
else
    TITLE="$(printf '%s' "$CMUS_DUMP" | grep 'file ' | awk -F/ '{print $NF}') "
fi

# Make track info string from artist and title (sanitized for non-ASCII chars)
TRACK=$(printf '%s' "${ARTIST%?} - $TITLE" | perl -pe 's/[^[:ascii:]]/?/g')

# OUTFILE format (by line number):
#   1 $SCROLL_COUNTER   (resets every 0.3s)
#   2 $SCROLL_OFFSET    (current scroll index)
#   3 $TRACK            (current track info)
OUTFILE="$HOME/.config/xmonad/script/.banner"

# Set banner scroll speed (higher == slower) and width (in chars)
SCROLL_SPEED=3
SCROLL_WIDTH=24

# If the original banner has changed, the song has changed, so reset the outfile.
# Otherwise, update the scroll counter and write it back to file
SCROLL_OFFSET=0
SCROLL_COUNTER=0
if [[ $(cat "$OUTFILE" | awk 'NR==3') != "$TRACK" ]]; then
    printf '%d\n' "$SCROLL_COUNTER" > "$OUTFILE"
    printf '%d\n' "$SCROLL_OFFSET" >> "$OUTFILE"
    printf '%s' "$TRACK" >> "$OUTFILE"
else
    SCROLL_COUNTER=$(awk 'NR==1' "$OUTFILE")
    SCROLL_COUNTER=$(expr $(expr $SCROLL_COUNTER + 1) % $SCROLL_SPEED)
    sed -i "1s/.*/$SCROLL_COUNTER/" "$OUTFILE"
    SCROLL_OFFSET=$(awk 'NR==2' "$OUTFILE")
fi

# Generate a scrollable banner from the track info.
# If the banner length is >= $SCROLL_WIDTH, the banner
# needs to scroll, so update the scroll index every
# $SCROLL_SPEED invocations.
if [[ ${#TRACK} > $SCROLL_WIDTH ]]; then
    if [[ $SCROLL_COUNTER = 0 ]]; then
        SCROLL_OFFSET=$(expr $(expr $SCROLL_OFFSET + 1) % ${#TRACK})
        sed -i "2s/.*/$SCROLL_OFFSET/" "$OUTFILE"
    fi
    BANNER="$TRACK $TRACK"
else
    BANNER=${TRACK}
fi
BANNER=${BANNER:$SCROLL_OFFSET:$SCROLL_WIDTH}
BANNER_PADDING=$(expr $(expr ${#BANNER} - $SCROLL_WIDTH ) / 2)
BANNER=$(printf '%*s%s%*s' $BANNER_PADDING "" "$BANNER" $BANNER_PADDING "")

# Progress bar
DURATION=$(printf '%s' "$CMUS_DUMP" | awk 'NR==3{print $2}')
POSITION=$(printf '%s' "$CMUS_DUMP" | awk 'NR==4{print $2}')
PROGRESS=$(bc -l <<< "$POSITION / $DURATION * 13")
PROGRESS=$(printf '%.0f' "$PROGRESS")
case $PROGRESS in
    0)
        PROGRESS='<------------->'
        ;;
    1)
        PROGRESS='<#------------>'
        ;;
    2)
        PROGRESS='<##----------->'
        ;;
    3)
        PROGRESS='<###---------->'
        ;;
    4)
        PROGRESS='<####--------->'
        ;;
    5)
        PROGRESS='<#####-------->'
        ;;
    6)
        PROGRESS='<######------->'
        ;;
    7)
        PROGRESS='<#######------>'
        ;;
    8)
        PROGRESS='<########----->'
        ;;
    9)
        PROGRESS='<#########---->'
        ;;
    10)
        PROGRESS='<##########--->'
        ;;
    11)
        PROGRESS='<###########-->'
        ;;
    12)
        PROGRESS='<############->'
        ;;
    13)
        PROGRESS='<#############>'
        ;;
esac       

printf '%s' "$STATUS $BANNER $PROGRESS"
