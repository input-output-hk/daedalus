### macOS

The icons with multiple levels of detail are in the `*.iconset`
directories.


### Windows

Use Nix to enter a shell with the necessary tools available:

    nix run nixpkgs.icoutils nixpkgs.imagemagick

To generate ico files:

    icotool -c -o 256x256.ico 256x256.png

To Merge all icons into a single bundle:

    ./update_icons

Currently, the Windows installer uses the bundle with 8 of the icons.


### Linux

The desktop launcher uses one of `{mainnet,staging,testnet}/1024x1024.png` (see `iconPath` in `../../default.nix`)
