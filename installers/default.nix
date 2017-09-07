with (import <nixpkgs> {}); 

with haskell.lib;

justStaticExecutables (haskell.packages.ghc802.callPackage ./cardano-installer.nix {})
