### macOS

The icons with multiple levels of detail are in the `*.iconset`
directories.


### Windows

To generate ico files:

    nix-shell -p icoutil

    icotool -c -o 256x256.ico 256x256.png

To Merge all icons into a single bundle:

    nix run nixpkgs.imagemagick

    ./update_icons

Currently, the Windows installer uses the bundle with 8 of the icons.


### Linux

The desktop launcher uses one of `{mainnet,staging,testnet}/1024x1024.png` (see `iconPath` in `../../default.nix`)
