{ stdenv, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, openssl, electron,
coreutils, utillinux, procps, cluster,
rawapp, master_config, cardanoPkgs, configFiles }:

let
  daedalus_frontend = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron}/bin/electron ${rawapp}
  '';
  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}
    set -x
    set -e

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
    ls -lh $DAEDALUS_DIR

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    if [ ! -d tls ]; then
      mkdir -p tls/{server,ca}
      ${openssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout tls/server/server.key -out tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp tls/server/server.crt tls/ca/ca.crt
    fi
    exec ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher --node ${cardanoPkgs.cardano-sl-wallet}/bin/cardano-node \
      --node-log-path logs/cardano-node.log \
      --db-path LDB/ \
      --wallet ${daedalus_frontend}/bin/daedalus \
      --launcher-logs-prefix logs/pub/ \
      --node-timeout 30 \
      --updater /not-supported \
      --configuration-file ${configFiles}/configuration.yaml \
      --configuration-key mainnet_wallet_macos64 \
      -n --report-server -n http://report-server.cardano-mainnet.iohk.io:8080 \
      -n --log-config -n ${configFiles}/log-config-prod.yaml \
      -n --keyfile -n Secrets/secret.key \
      -n --db-path -n DB/ \
      -n --wallet-db-path -n Wallet/ \
      -n --no-ntp \
      -n --tlscert -n tls/server/server.crt \
      -n --tlskey -n tls/server/server.key \
      -n --tlsca -n tls/ca/ca.crt \
      -n --configuration-file -n ${configFiles}/configuration.yaml \
      -n --configuration-key -n mainnet_wallet_macos64 \
      -n --wallet-address -n 127.0.0.1:8090 \
      -n --topology -n ${configFiles}/topology.yaml
  '';
in daedalus
