{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-windows";

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

  package = unsignedInstaller; # FIXME: this is wrong

  unsignedInstaller = oldCode.unsigned-windows-installer;

  makeSignedInstaller = oldCode.pkgs.writeScriptBin "make-signed-installer" ''
    echo >&2 'fatal: not yet implemented'
    exit 1
  '';

}
