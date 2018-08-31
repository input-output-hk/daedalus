let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, cluster ? "staging"
, autoStartBackend ? false
}:

let
  daedalusPkgs = import ./. { inherit cluster; };
  yarn = pkgs.yarn.override { inherit nodejs; };
  nodejs = pkgs.nodejs-8_x;
  fixYarnLock = pkgs.stdenv.mkDerivation {
    name = "fix-yarn-lock";
    buildInputs = [ nodejs yarn pkgs.git ];
    shellHook = ''
      git diff > pre-yarn.diff
      yarn
      git diff > post-yarn.diff
      diff pre-yarn.diff post-yarn.diff > /dev/null
      if [ $? != 0 ]
      then
        echo "Changes by yarn have been made. Please commit them."
      else
        echo "No changes were made."
      fi
      rm pre-yarn.diff post-yarn.diff
      exit
    '';
  };
  daedalusShell = pkgs.stdenv.mkDerivation {
    name = "daedalus";
    buildInputs = [ nodejs yarn ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl electron
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
    ] ++ (lib.optionals autoStartBackend [
      daedalusPkgs.daedalus-bridge
    ]));
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
  };
  daedalus = daedalusShell.overrideAttrs (oldAttrs: {
    shellHook = ''
       if [ ! -f "$CARDANO_TLS_PATH/ca.crt" ]
       then
         echo "CARDANO_TLS_PATH must be set"
         exit 1
       fi
      ${oldAttrs.shellHook}
      yarn dev
      exit 0
    '';
  });
in daedalusShell // { inherit fixYarnLock; }
