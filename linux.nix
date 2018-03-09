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
  launcherConfig = writeText "launcher-config.json" (builtins.toJSON {
    nodePath = "${cardanoPkgs.cardano-sl-wallet}/bin/cardano-node";
    nodeArgs = [
      "--update-latest-path" "$HOME/.local/share/Daedalus/mainnet/installer.sh"
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
    ];
    nodeDbPath = "DB/";
    nodeLogConfig = "${configFiles}/daedalus.yaml";
    nodeLogPath = "$HOME/.local/share/Daedalus/mainnet/logs/cardano-node.log";
    reportServer = "http://report-server.cardano-mainnet.iohk.io:8080";
    configuration = {
      filePath = "${configFiles}/configuration.yaml";
      key = "mainnet_wallet_macos64";
      systemStart = null;
      seed = null;
    };
    updaterPath = "/bin/update-runner";
    updateArchive = "$HOME/.local/share/Daedalus/mainnet/installer.sh";
    updateWindowsRunner = null;
    nodeTimeoutSec = 30;
    launcherLogsPrefix = "$HOME/.local/share/Daedalus/mainnet/logs/";
    walletPath = "${daedalus_frontend}/bin/daedalus";
    walletArgs = [];
  });
  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}
    set -x
    set -e

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    if [ ! -d tls ]; then
      mkdir -p tls/{server,ca}
      ${openssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout tls/server/server.key -out tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp tls/server/server.crt tls/ca/ca.crt
    fi
    exec ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher \
      --config ${launcherConfig}
  '';
in daedalus
