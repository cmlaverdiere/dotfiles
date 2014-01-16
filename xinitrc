# Give functionality to volume buttons.
xbindkeys

# Load wallpaper
feh --bg-fill ~/pictures/walls/xmonad-wall-large.png

# Enable compositing
xcompmgr -c -C -f -r 10 -D 5 &

# For my eyes late at night.
redshift &

# Handle automounting of usb / cd
udiskie --tray &

# Set xmonad cursor to normal.
xsetroot -cursor_name left_ptr

# Start various environments. (uncomment the one you want) #
# exec awesome
exec xmonad
