# Give functionality to volume buttons.
xbindkeys

# Load wallpaper
# feh --bg-fill ~/pictures/walls/pic.jpg

# Enable compositing
# xcompmgr -c -C -f -r 10 -D 5 &

# For my eyes late at night.
redshift &

# Handle automounting of usb / cd
# udiskie --tray &

# Clipboard management
# autocutsel -fork &
# autocutsel -selection PRIMARY -fork &

# Remap ctrl key to caps lock.
setxkbmap -option ctrl:rctrl
setxkbmap -option ctrl:swapcaps
setxkbmap -option ctrl:nocaps

# Compose key
# setxkbmap -option compose:rctrl

# Set keyboard rate.
xset r rate 200 50

# Hide cursor when inactive
unclutter &

# Caps lock as escape when pressed
# xcape -e 'Control_L=Escape'

# Start xfce
# exec startxfce4

# Start i3
exec i3
