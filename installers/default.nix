with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/48ecdcf5980a6504cd3b884b121e29efb2fb83dc.tar.gz) { config = {}; }); 

with haskell.lib;

justStaticExecutables (haskell.packages.ghc802.callPackage ./cardano-installer.nix {})
