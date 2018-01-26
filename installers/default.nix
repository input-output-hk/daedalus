with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/ffea68a09cedab941d19f02c9041689ebc81192e.tar.gz) { config = {}; });

with haskell.lib;

justStaticExecutables (haskell.packages.ghc802.callPackage ./cardano-installer.nix {})
