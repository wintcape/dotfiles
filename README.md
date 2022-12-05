Backups of configuration files for my makeshift home desktop environment.
<br><br>
For a two monitor setup.

## Defaults
- **Terminal**: Alacritty
- **Editor**: neovim
- **Browser**: Mozilla Firefox
- **Audio controller**: pulsemixer
- **Font**: AcPlus IBM VGA 8x16<br>(must be pre-installed, see `fonts/`)

## Dependencies
- X
- pulseaudio
- dbus
- sudo

## To build and launch
```
$ <path>/xmonad/build && startx
```

## Supported applications
- Alacritty
- neovim
- Mozilla Firefox
- pulsemixer
- cmus
- htop
- Thunar
- Qalculate-gtk
- VLC Media Player
- OBS Studio
- Audacity
- Minecraft
- [nbted](https://github.com/C4K3/nbted) (must be pre-installed) and `mcfunction` (builtin) plugins for neovim<br>(for editing Minecraft game files in neovim)
- Steam<br>(must be pre-installed in chroot environment)
