{ stdenv, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, openssl, xar, cpio, electron }:
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
    cd ~/.daedalus/
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
  topologyFile = writeText "topology.yaml" ''
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
    mkdir -p ~/.daedalus/Secrets-${version}/
    if [ ! -d ~/.daedalus/tls ]; then
      mkdir -p ~/.daedalus/tls/{server,ca}
      ${openssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout ~/.daedalus/tls/server/server.key -out ~/.daedalus/tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp ~/.daedalus/tls/server/server.crt ~/.daedalus/tls/ca/ca.crt
    fi
    exec ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher --node ${cardanoPkgs.cardano-sl-wallet}/bin/cardano-node \
      --node-log-path ~/.daedalus/logs/cardano-node.log \
      --db-path ~/.daedalus/LDB-${version}/ \
      --wallet ${daedalus_frontend}/bin/daedalus \
      --launcher-logs-prefix ~/.daedalus/logs/pub/ \
      --node-timeout 30 \
      --updater notsupported \
      --configuration-file ${configFiles}/configuration.yaml \
      --configuration-key mainnet_wallet_macos64 \
      -n --report-server -n http://report-server.cardano-mainnet.iohk.io:8080 \
      -n --log-config -n ${configFiles}/log-config-prod.yaml \
      -n --keyfile -n ~/.daedalus/Secrets-${version}/secret.key \
      -n --db-path -n ~/.daedalus/DB-${version}/ \
      -n --wallet-db-path -n ~/.daedalus/Wallet-${version}/ \
      -n --no-ntp \
      -n --tlscert -n ~/.daedalus/tls/server/server.crt \
      -n --tlskey -n ~/.daedalus/tls/server/server.key \
      -n --tlsca -n ~/.daedalus/tls/ca/ca.crt \
      -n --configuration-file -n ${configFiles}/configuration.yaml \
      -n --configuration-key -n mainnet_wallet_macos64 \
      -n --wallet-address -n 127.0.0.1:8090 \
      -n --topology -n ${configFiles}/topology.yaml
  '';
in {
  inherit daedalus rawapp;
}
