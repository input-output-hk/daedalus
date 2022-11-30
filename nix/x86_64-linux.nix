{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-linux";

let

  sourceLib = import ./source-lib.nix { inherit inputs; };

  oldCode = import ./old-default.nix {
    target = targetSystem;
    localLibSystem = targetSystem;
    inherit cluster sourceLib;
  };

  inherit (oldCode) pkgs;
  inherit (pkgs) lib;

in rec {

  inherit oldCode;

  package = oldCode.daedalus;

  unsignedInstaller = oldCode.wrappedBundle;

  makeSignedInstaller = throw "We don’t sign binary files inside installers for ‘${targetSystem}’, you’re good.";

}
