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
- [vim-plug](https://github.com/junegunn/vim-plug)<br>(if using neovim defaults)
- xmonad*<br>(only used as a launcher for a custom build of `xmonad-X86_64-linux`. Not needed if using custom launcher)

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
- [coc.nvim](https://github.com/neoclide/coc.nvim)<br>(language servers for Haskell, C/C++, bash, Typescript)
- [nbted](https://github.com/C4K3/nbted) (must be pre-installed) and `mcfunction` (builtin) plugins for neovim<br>(for editing Minecraft game files in neovim)
- Steam<br>(must be pre-installed in chroot environment)
