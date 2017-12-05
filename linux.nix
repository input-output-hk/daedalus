with import (import ./fetchNixpkgs.nix {
  rev = "3eccd0b11d1";
  sha256 = "1z5zp60dlr61748nlcjlka94v02misn0z3d6gb44k7c8gbi7kkmi";
}) { config = {}; };
let
  darwinPackage = fetchurl {
    url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.3253.pkg";
    sha256 = "13992425b5408abb60e75bcf8ae01916c2d214d921f68d923eae594ef7a080d6";
  };
  rawapp = pkgs.runCommand "daedalus-app" { buildInputs = with pkgs; [ xar cpio ]; } ''
    xar -xf ${darwinPackage}
    cat $NIX_BUILD_TOP/temp.pkg/Payload | gunzip | cpio -i
    cp -vir Daedalus.app/Contents/Resources/app/ $out
  '';
  daedalus_frontend = pkgs.writeScriptBin "daedalus" ''
    #!${pkgs.stdenv.shell}
    cd ~/.daedalus/
    exec ${pkgs.electron}/bin/electron ${rawapp}
  '';
  cardanoSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-sl";
    rev = "4b518a9db8cf171d8f509aaf3e5c8cc2dc804815";
    sha256 = "0d33d69fscyj21ad3sh48vq95sj2d2j7abj02s1pnfrfhbcmbmyg";
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
  topologyFile = pkgs.writeText "topology.yaml" ''
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
in daedalus
