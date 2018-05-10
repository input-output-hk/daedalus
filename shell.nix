let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, cluster ? "staging"
}:

with pkgs;

let
  daedalusPkgs = import ./. { inherit cluster; };
in stdenv.mkDerivation {
  name = "daedalus";
  LAUNCHER_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml";
  DAEDALUS_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/";
  shellHook = ''
    ln -svf $(type -P cardano-node)
    for x in wallet-topology.yaml configuration.yaml mainnet-genesis-dryrun-with-stakeholders.json ; do
        ln -svf ${daedalusPkgs.daedalus.cfg}/etc/$x
    done
    mkdir -p Secrets
  '';

  buildInputs = [
    nix bash binutils coreutils curl gnutar
    git python27 curl electron nodejs-8_x
    nodePackages.node-gyp nodePackages.node-pre-gyp
    gnumake yarn
    daedalusPkgs.daedalus-bridge
  ];
}
