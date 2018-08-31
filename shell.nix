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
  launcher-config = builtins.fromJSON (builtins.readFile (pkgs.runCommand "read-launcher-config" { buildInputs = [ pkgs.yaml2json ]; } "cat ${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml|yaml2json > $out"));
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
    ] ++ (localLib.optionals autoStartBackend [
      daedalusPkgs.daedalus-bridge
    ]));
    LAUNCHER_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml";
    DAEDALUS_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/";
    DAEDALUS_DIR = "./";
    CLUSTER = cluster;
    shellHook = ''
      ${localLib.optionalString pkgs.stdenv.isLinux "export XDG_DATA_HOME=$HOME/.local/share"}
      ln -svf $(type -P cardano-node)
      for x in wallet-topology.yaml configuration.yaml mainnet-genesis-dryrun-with-stakeholders.json ; do
          ln -svf ${daedalusPkgs.daedalus.cfg}/etc/$x
      done
      mkdir -p Secrets ${cluster}
        ${localLib.optionalString autoStartBackend ''
          mkdir -p "${launcher-config.tlsPath}/server" "${launcher-config.tlsPath}/client"
          cardano-x509-certificates \
          --server-out-dir "${launcher-config.tlsPath}/server" \
          --clients-out-dir "${launcher-config.tlsPath}/client" \
          --configuration-file ${daedalusPkgs.daedalus.cfg}/etc/configuration.yaml \
          --configuration-key mainnet_dryrun_full
          echo ${launcher-config.tlsPath}
        ''
      }
      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${nodejs}/include/node"
      yarn install
      ln -svf ${pkgs.electron}/bin/electron ./node_modules/electron/dist/electron
      ${localLib.optionalString (! autoStartBackend) ''
      echo "Instructions for manually running cardano-node:"
      echo "In cardano repo run scripts/launch/demo-nix.sh"
      echo "export CARDANO_TLS_PATH=/path/to/cardano-sl/state-demo/tls/client"
      echo "yarn dev"
      ''}
    '';
  };
  daedalus = daedalusShell.overrideAttrs (oldAttrs: {
    shellHook = ''
       if [ ! -f "$CARDANO_TLS_PATH/ca.crt" ] || [ ! -f "tls/client/ca.crt" ]
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
