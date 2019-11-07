{ system ? builtins.currentSystem
, config ? {}
, localLib ? import ../lib.nix {}
, pkgs ? import localLib.sources.nixpkgs { inherit system config; }
}:

let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = import ./overlays/required.nix { inherit pkgs; };
  };

in haskellPackages // { inherit pkgs; }
