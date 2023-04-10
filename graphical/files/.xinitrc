#!/bin/sh

# nvidia
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --dpi 96


userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
userkeymap=$HOME/.Xkeymap-laptop
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -f "$userkeymap" ]; then
   test -f $userkeymap && xkbcomp $userkeymap $DISPLAY
fi

# start programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

#keepassxc &
#nextcloud &
hsetroot -solid "#282828" &
xcompmgr -c -l0 -t0 -r0 -o.00 &
#caffeine &
exec i3