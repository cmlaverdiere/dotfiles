# Give functionality to volume buttons.
xbindkeys

# For my eyes late at night.
redshift &

# Compose key
# setxkbmap -option compose:ralt
# setxkbmap -option compose:rctrl

# Remap ctrl key to caps lock.
setxkbmap -option ctrl:rctrl
setxkbmap -option ctrl:swapcaps
setxkbmap -option ctrl:nocaps

# Set keyboard / mouse rate.
xset r rate 175 50
xset m 3/2 0

# Hide cursor when inactive
unclutter &

# Clipboard management
parcellite &

# Dropbox syncing
dropbox &

# Caps lock as escape when pressed
# xcape -e 'Control_L=Escape'

# XFCE startup
# exec startxfce4

# i3 startup
# sleep 2 && xrandr --output HDMI1 --auto --left-of LVDS1
sleep 2 && xrandr --output LVDS1 --pos 1920x600 --output HDMI1 --mode 1920x1200 --pos 0x0
# compton --no-fading-openclose &
compton --inactive-opacity 0.95 --active-opacity 0.95 &
# compton &
nitrogen --restore
# xrandr --setprovideroffloadsink "TURKS @ pci:0000:01:00.0" Intel
xfce4-power-manager
exec i3
