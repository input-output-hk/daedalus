{ system ? builtins.currentSystem
, config ? {}
, localLib ? import ../lib.nix {}
, pkgs
, daedalus-bridge
}:

let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = import ./overlays/required.nix { inherit pkgs; };
  };
  #haskellPackages = haskellPackages1.extend (import ./overlays/add-test-stubs.nix { inherit pkgs daedalus-bridge; });

in haskellPackages // { inherit pkgs; }
