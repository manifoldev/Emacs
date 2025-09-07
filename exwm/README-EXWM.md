EXWM as a full desktop (Doom Emacs)
===================================

Quick start
-----------

1) Install the packages you use in this config (example):
   - exwm, exwm-evil, picom, trayer, dunst, nm-applet, blueman, xss-lock,
     betterlockscreen, playerctl, brightnessctl, autorandr (optional).

2) Display manager login (GDM/LightDM/SDDM):
   - Copy `exwm/exwm.desktop` to either:
     - system-wide: `/usr/share/xsessions/` (root required), or
     - user: `~/.local/share/xsessions/`
   - Select “EXWM (Doom Emacs)” at the login screen.

3) startx users:
   - Copy `exwm/xinitrc.exwm` to `~/.xinitrc` (overwrite if intended).
   - Run `startx` to launch EXWM.

Notes
-----

- `config.el` calls `(exwm-enable)` and enables RandR; display changes trigger
  `autorandr --change` if available.
- Tray applets and background services are started via `exwm-init-hook`.
- A Polkit auth agent is started if found (lxqt/polkit-gnome/kde variants).
- Audio/brightness/media keys are bound (pactl/brightnessctl/playerctl).

