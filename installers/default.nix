{ system ? builtins.currentSystem
, config ? {}
, pkgs
, daedalus-bridge
}:

let
  haskellPackages = pkgs.haskell.packages.ghc8107.override {
    overrides = import ./overlays/required.nix { inherit pkgs; };
  };
  #haskellPackages = haskellPackages1.extend (import ./overlays/add-test-stubs.nix { inherit pkgs daedalus-bridge; });

in haskellPackages // { inherit pkgs; }
