# Things common between all OS-es, that build on all platforms.

{ inputs, targetSystem, cluster }:

rec {

  sourceLib = import ./source-lib.nix { inherit inputs; };

  oldCode = import ./old-default.nix {
    target = targetSystem;
    localLibSystem = targetSystem;
    inherit cluster sourceLib;
  };

  inherit (oldCode) pkgs;

}
