{ ... }:
let
  flake-compat = import (import ./nix/sources.nix).flake-compat;
  flake = (flake-compat { src = ./.; }).defaultNix;
in
# XXX: remove `required` to prevent building installers on Hydra, as they take too much space there – we only build them on Cicero:
__mapAttrs (_: xs: removeAttrs xs ["required"]) flake.hydraJobs // { recurseForDerivations = {}; }
