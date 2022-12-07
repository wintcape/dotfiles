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
- [vim-plug](https://github.com/junegunn/vim-plug) (if using neovim defaults)
- xmonad* (only used as a launcher for a custom build of `xmonad-X86_64-linux`. Not needed if using custom launcher)
- [stack](https://github.com/commercialhaskell/stack) (for build)

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
- [coc.nvim](https://github.com/neoclide/coc.nvim) (language servers supported for Haskell, C/C++, bash, Typescript)
- [nbted](https://github.com/C4K3/nbted) (must be pre-installed) and [mcfunction](https://minecraft.fandom.com/wiki/Function_(Java_Edition)) (builtin) plugins for neovim<br>(for editing Minecraft game files in neovim)
- Steam (must be pre-installed in chroot environment)

## Preparing the environment
1. Extract the repository into the desired `$HOME/.config` directory on the host system.
2. Move `.bash_profile`, `.bashrc`, and `.xinitrc` into the `$HOME` directory.
3. Edit `$HOME/.config/xmonad/src/Config/Defaults.hs` to match `$HOME` path and desired defaults.

## Building XMonad
The main application that acts as the window manager for X is a custom build of [XMonad](https://xmonad.org/).<br>
1. Edit `./xmonad/src/Config/Defaults.hs` to match `$HOME` path and desired defaults.
2. Perform the build:
```
$HOME/.config/xmonad/build
```

## Launching the environment
1. Edit `.bash_profile`, `.bashrc`, and `.xinitrc` to match host system hardware and desired defaults.
2. Start X:
```
startx
```

## Viewing and configuring keybindings
1. Edit the value of `myKeyBindings` in `$HOME/.config/xmonad/src/xmonad.hs`.
2. Rebuild and relaunch XMonad.<br>(default keybinding for this is `<Super>+<Escape>`)

## Configuring neovim
If using the provided neovim configuration, it will not work the first time it launches. The following command must be run first to install the plugins referenced in `$HOME/.config/nvim/init.vim`:
```
:PlugInstall
```

## Support for Steam
I run Steam inside a chroot environment, and I have two default keybindings to enter it:
- `<Super>+<F1>`: mount, chroot, launch Steam, unmount on exit 
- `<Super>+<F2>`: mount, chroot, launch Alacritty (for editing chroot environment), unmount on exit
<br>
For these to work:

1. Steam will need to be prepared in a chroot environment.
2. The scripts in `$HOME/.config/steam` will need to be modified for the target architecture, operating system, and home / chroot directories.
3. The scripts in `$HOME/.config/steam` must be executable via `sudo` and added to the `$PATH` variable.
