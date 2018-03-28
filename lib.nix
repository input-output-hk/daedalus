let
  fetchNixPkgs = import ./fetch-nixpkgs.nix;
  pkgs = import fetchNixPkgs {};
  lib = pkgs.lib;
in lib // (rec {
  inherit fetchNixPkgs;
})
