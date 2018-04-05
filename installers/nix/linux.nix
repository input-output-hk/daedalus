{ stdenv, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, openssl, electron,
coreutils, utillinux, procps, cluster,
rawapp, daedalus-bridge }:

let
  # closure size TODO list
  # electron depends on cups, which depends on avahi
  daedalus_frontend = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron}/bin/electron ${rawapp}/share/daedalus/main/
  '';
  slimOpenssl = runCommand "openssl" {} ''
    mkdir -pv $out/bin/
    cp ${openssl}/bin/openssl $out/bin/
  '';
  configFiles = runCommand "cardano-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${daedalus-bridge}/config/configuration.yaml configuration.yaml
    cp -vi ${daedalus-bridge}/config/mainnet-genesis-dryrun-with-stakeholders.json mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${daedalus-bridge}/config/mainnet-genesis.json mainnet-genesis.json
    cp -vi ${daedalus-bridge}/config/log-config-prod.yaml daedalus.yaml
    cp -vi ${topologies.${cluster}} topology.yaml
  '';
  topologies = {
    # TODO DEVOPS-690 integration
    mainnet = writeText "topology.yaml" ''
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
    staging = writeText "topology.yaml" ''
      wallet:
        relays:
          [
            [
              { host: relays.awstest.iohkdev.io }
            ]
          ]
        valency: 1
        fallbacks: 7
    '';
  };
  perClusterConfig = {
    mainnet = {
      key = "mainnet_wallet_macos64";
    };
    staging = {
      key = "mainnet_dryrun_wallet_macos64";
    };
  };
  launcherConfig = writeText "launcher-config.json" (builtins.toJSON {
    nodePath = "${daedalus-bridge}/bin/cardano-node";
    nodeArgs = [
      "--update-latest-path" "$HOME/.local/share/Daedalus/${cluster}/installer.sh"
      "--keyfile" "Secrets/secret.key"
      "--wallet-db-path" "Wallet/"
      "--update-server" "https://update-cardano-mainnet.iohk.io"
      "--update-with-package"
      "--no-ntp"
      "--tlscert" "tls/server/server.crt"
      "--tlskey" "tls/server/server.key"
      "--tlsca" "tls/ca/ca.crt"
      "--topology" "${configFiles}/topology.yaml"
      "--wallet-address" "127.0.0.1:8090"
      "--logs-prefix" "Logs"
    ];
    nodeDbPath = "DB/";
    nodeLogConfig = "${configFiles}/daedalus.yaml";
    nodeLogPath = "$HOME/.local/share/Daedalus/${cluster}/Logs/cardano-node.log";
    reportServer = "http://report-server.cardano-mainnet.iohk.io:8080";
    configuration = {
      filePath = "${configFiles}/configuration.yaml";
      key = perClusterConfig.${cluster}.key;
      systemStart = null;
      seed = null;
    };
    updaterPath = "/bin/update-runner";
    updateArchive = "$HOME/.local/share/Daedalus/${cluster}/installer.sh";
    updateWindowsRunner = null;
    nodeTimeoutSec = 30;
    launcherLogsPrefix = "$HOME/.local/share/Daedalus/${cluster}/Logs/";
    walletPath = "${daedalus_frontend}/bin/daedalus";
    walletArgs = [];
  });
  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    set -xe

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export CLUSTER=${cluster}
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{Logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    if [ ! -d tls ]; then
      mkdir -p tls/{server,ca}
      ${slimOpenssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout tls/server/server.key -out tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp tls/server/server.crt tls/ca/ca.crt
    fi
    exec ${daedalus-bridge}/bin/cardano-launcher \
      --config ${launcherConfig}
  '';
in daedalus
