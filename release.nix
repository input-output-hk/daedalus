{ ... }:
let
  flake-compat = import (import ./nix/sources.nix).flake-compat;
  flake = (flake-compat { src = ./.; }).defaultNix;
in
flake.hydraJobs // { recurseForDerivations = {}; }
