# Things common between all OS-es, that build on all platforms.

{ inputs, targetSystem, cluster }:

rec {

  sourceLib = import ./source-lib.nix { inherit inputs; };

  oldCode = import ./old-default.nix {
    target = targetSystem;
    inherit inputs cluster sourceLib;
  };

  inherit (oldCode) pkgs;

}
