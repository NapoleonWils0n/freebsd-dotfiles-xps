# ~/.zshenv

# Path
typeset -U PATH path
path=("$HOME/bin" "/usr/local/bin" "$path[@]")
export PATH

# xdg directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# XDG_RUNTIME_DIR automatically set to /var/run/xdg/djwilcox

# qt5
export QT_QPA_PLATFORMTHEME=qt5ct

# ssh-add
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# less
export LESSHISTFILE="${XDG_CONFIG_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/keys"

# vi mode
export KEYTIMEOUT=1

# mpd host variable for mpc
export MPD_HOST="/home/djwilcox/.config/mpd/socket"

# dark theme needed for handbrake
export GTK_THEME=Adwaita-dark:dark
