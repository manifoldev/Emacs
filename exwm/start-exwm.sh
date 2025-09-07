#!/usr/bin/env bash
set -euo pipefail

# Minimal EXWM session launcher for Doom Emacs
# - Starts a fresh D-Bus session
# - Maximizes the first Emacs frame to cover the screen

export XDG_CURRENT_DESKTOP=EXWM
export DESKTOP_SESSION=exwm

# Optional: set a GTK theme for apps (comment out if undesired)
# export GTK_THEME=Adwaita:dark

exec dbus-run-session emacs -mm --debug-init

