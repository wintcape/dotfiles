Backups of configuration files for my makeshift home desktop environment.
<br><br>
For a two monitor setup.

## Defaults
- **Terminal**: Alacritty
- **Editor**: neovim
- **Browser**: Mozilla Firefox
- **Audio controller**: pulsemixer
- **Font**: AcPlus IBM VGA 8x16<br>(must be pre-installed, see `fonts/`)
Edit `./xmonad/src/Config/Defaults.hs` to modify.

## Dependencies
- X
- pulseaudio
- dbus
- [vim-plug](https://github.com/junegunn/vim-plug)<br>(if using neovim defaults)
- xmonad*<br>(only used as a launcher for a custom build of `xmonad-X86_64-linux`. Not needed if using custom launcher)
- [stack](https://github.com/commercialhaskell/stack)<br>(for build)

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
- [coc.nvim](https://github.com/neoclide/coc.nvim)<br>(language servers supported for Haskell, C/C++, bash, Typescript)
- [nbted](https://github.com/C4K3/nbted) (must be pre-installed) and `mcfunction` (builtin) plugins for neovim<br>(for editing Minecraft game files in neovim)
- Steam<br>(must be pre-installed in chroot environment)

## Building the environment
The main application that acts as the window manager for X is a custom build of xmonad.<br><br>

1. Edit `./xmonad/src/Config/Defaults.hs` to match `$HOME` path and desired defaults.
2. Perform the build:
```
./xmonad/build
```

## Launching the environment
1. Make sure `.xinitrc` and `.bash_profile` are placed in `$HOME` directory.
2. Start X:
```
startx
```

## Viewing and configuring keybindings
1. Edit the value of `myKeyBindings` in `./xmonad/src/xmonad.hs`.
2. Rebuild and relaunch XMonad.<br>(default keybinding for this is `<Super>+<Escape>`)

## Configuring neovim
If using the provided neovim configuration, it will not work the first time it launches. The following command must be run first to install the plugins referenced in `init.vim`:
```
:PlugInstall
```

## Support for Steam
I run Steam inside a chroot environment, and I have two default keybindings to enter it via `sudo`:
- `<Super>+<F1>`: chroot, launch Steam, unchroot on exit 
- `<Super>+<F2>`: chroot, launch Alacritty (for editing chroot environment), unchroot on exit
For these to work, Steam will need to be prepared in the chroot environment and the scripts in `./steam` will need to be modified for the target architecture and operating system.
