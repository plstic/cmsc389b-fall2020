# Visual Studio Code

## Line-Endings

We recommend `LF` line-endings.
To change this:
1. `File > Preferences > Settings` (or do `ctrl+,`)
1. `Text Editor > Files` (or search `eol`)
1. Scroll to the `Eol` setting, and choose `\n`

Need to change an entire directory to LF?
```shell
sudo apt install dos2unix
find -type f -print0 | xargs -0 dos2unix
```

## Extensions

They're super easy to install.
You can click on the four-box icon in the left-side taskbar to access your extensions (or do `ctrl+shift+x`).
From there, you can manage your extensions (un/install, view docs, etc).
The main file icon (or `ctrl+shift+e`) takes you back to the left-side file explorer.
