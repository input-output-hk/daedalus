{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-windows";

let

  newCommon = import ./new-common.nix { inherit inputs targetSystem cluster; };

  inherit (newCommon) sourceLib oldCode pkgs;
  inherit (pkgs) lib;

in rec {

  inherit newCommon oldCode;

  package = unsignedInstaller; # FIXME: this is wrong

  unsignedInstaller = oldCode.unsigned-windows-installer;

  makeSignedInstaller = oldCode.pkgs.writeScriptBin "make-signed-installer" ''
    echo >&2 'fatal: not yet implemented'
    exit 1
  '';

}
