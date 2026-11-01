if [[ -z $WAYLAND_DISPLAY && -z $DISPLAY && $XDG_VTNR == 1 && ! -e ~/.no-niri && -z $NIRI_SESSION_STARTED ]]; then
    export NIRI_SESSION_STARTED=1
    exec niri-session
fi
