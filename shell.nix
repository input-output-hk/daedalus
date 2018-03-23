let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

with pkgs;

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [
    nix bash binutils coreutils curl gnutar
    git python27 curl electron nodejs-6_x
    nodePackages.node-gyp nodePackages.node-pre-gyp
    gnumake
  ];

  src = null;

}
