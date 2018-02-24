{ stdenv, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, openssl, xar, cpio, electron,
coreutils, utillinux, procps, cluster ? "mainnet" }:

let
  master_config = {
    daedalus_build_number = "3619";
    cardano_rev = "0c1fab91";
    daedalus_hash = "0w0pz93yz380xrizlsbzm9wisnf99s6z2jq0ph9xqap9cpjlyr7x";
    cardano_hash = "1z2yjkm0qwhf588qnxcbz2d5mxvhqdxawwl8dczfnl47rb48jm52";
  };
  darwinPackage = fetchurl {
    url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.${master_config.daedalus_build_number}.pkg";
    sha256 = master_config.daedalus_hash;
  };
  rawapp = runCommand "daedalus-app" { buildInputs = [ xar cpio ]; } ''
    xar -xf ${darwinPackage}
    cat $NIX_BUILD_TOP/temp.pkg/Payload | gunzip | cpio -i
    cp -vir Daedalus.app/Contents/Resources/app/ $out
  '';
  daedalus_frontend = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron}/bin/electron ${rawapp}
  '';
  cardanoSrc = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-sl";
    rev = master_config.cardano_rev;
    sha256 = master_config.cardano_hash;
  };
  cardanoPkgs = import cardanoSrc { gitrev = cardanoSrc.rev; };
  version = "1.0";
  configFiles = runCommand "cardano-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${cardanoPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
    cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
    cp -vi ${cardanoPkgs.cardano-sl.src + "/../log-config-prod.yaml"} log-config-prod.yaml
    cp -vi ${topologyFile} topology.yaml
  '';
  topologyFile = topologies.${cluster};
  topologies.mainnet = writeText "topology.yaml" ''
    wallet:
      relays:
        [
          [
            { host: relays.cardano-mainnet.iohk.io }
          ]
        ]
      valency: 1
      fallbacks: 7
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
