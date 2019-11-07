{ pkgs }:
let
  arch = "x86"; # "amd64"; -- amd64, because we are using mingwW64
  buildGcc = pkgs.gcc;
  crossPkgs = import pkgs.path {
    crossSystem = pkgs.lib.systems.examples."${if arch == "x86" then "mingw32" else "mingwW64"}";
    localSystem.system = pkgs.system;
  };
  nsis = crossPkgs.callPackage ./nsis-inner.nix { inherit arch buildGcc; };
in nsis
