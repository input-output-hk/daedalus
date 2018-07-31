### macOS

The icons with multiple levels of detail are in the `*.iconset`
directories.


### Windows

To generate ico files:

    nix-shell -p icoutil

    icotool -c -o 256x256.ico 256x256.png

Currently, the Windows installer uses only the 64x64 icon.


### Linux

The desktop launcher uses one of `{mainnet,staging,testnet}/1024x1024.png`.
