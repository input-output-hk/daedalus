with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/ffea68a09cedab941d19f02c9041689ebc81192e.tar.gz) { config = {}; });

with haskell.lib;

let
  haskellPackages = haskell.packages.ghc802.override {
    overrides = self: super: {
      dhall-json = self.callPackage ./dhall-json.nix {};
      dhall = doJailbreak (self.callPackage ./dhall-haskell.nix {});
    };
  };

in

  justStaticExecutables (haskellPackages.callPackage ./cardano-installer.nix {})
