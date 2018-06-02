let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, cluster ? "staging"
, autoStartBackend ? false
}:

with pkgs;

let
  daedalusPkgs = import ./. { inherit cluster; };
in stdenv.mkDerivation ({
  name = "daedalus";

  buildInputs = [
    nix bash binutils coreutils curl gnutar
    git python27 curl electron nodejs-8_x
    nodePackages.node-gyp nodePackages.node-pre-gyp
    gnumake yarn
  ] ++ (lib.optionals autoStartBackend [
    daedalusPkgs.daedalus-bridge
  ]);
} // lib.optionalAttrs autoStartBackend {
  LAUNCHER_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml";
  DAEDALUS_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/";
  DAEDALUS_DIR = "./";
  CLUSTER = cluster;
  shellHook = ''
    ln -svf $(type -P cardano-node)
    for x in wallet-topology.yaml configuration.yaml mainnet-genesis-dryrun-with-stakeholders.json ; do
        ln -svf ${daedalusPkgs.daedalus.cfg}/etc/$x
    done
    mkdir -p Secrets ${cluster}
  '';
})
