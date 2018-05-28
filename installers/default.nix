let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

with pkgs;
with haskell.lib;

let
  haskellPackages = haskell.packages.ghc822.override {
    overrides = self: super: {
      dhall-json = self.callPackage ./dhall-json.nix {};
      dhall = doJailbreak (self.callPackage ./dhall-haskell.nix {});
      github = self.callPackage ./github.nix {};
    };
  };

in

  justStaticExecutables (haskellPackages.callPackage ./daedalus-installer.nix {})
