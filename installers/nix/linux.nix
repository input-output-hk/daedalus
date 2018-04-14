{ stdenv, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, openssl, electron,
coreutils, utillinux, procps, cluster,
rawapp, daedalus-bridge, daedalus-installer }:

let
  slimOpenssl = runCommand "openssl" {} ''
    mkdir -pv $out/bin/
    cp ${openssl}/bin/openssl $out/bin/
  '';
  daedalus-config = runCommand "daedalus-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${daedalus-bridge}/config/configuration.yaml configuration.yaml
    ## TODO: we don't need both of those genesis files (even if file names sound cool),
    ##       but the choice would have to be made in the Dhall-generated files,
    ##       splitting the dep chain further:
    cp -vi ${daedalus-bridge}/config/mainnet-genesis-dryrun-with-stakeholders.json mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${daedalus-bridge}/config/mainnet-genesis.json mainnet-genesis.json
    cp -vi ${daedalus-bridge}/config/log-config-prod.yaml daedalus.yaml
    ${daedalus-installer}/bin/make-installer --cluster ${cluster} config "${daedalus-installer.src}/dhall" "."
  '';
  # closure size TODO list
  # electron depends on cups, which depends on avahi
  daedalus-frontend = writeScriptBin "daedalus-frontend" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron}/bin/electron ${rawapp}/share/daedalus/main/
  '';
  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    set -xe

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export           CLUSTER=${cluster}
    export      DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
    export   DAEDALUS_BRIDGE=${daedalus-bridge}
    export   DAEDALUS_CONFIG=${daedalus-config}
    export DAEDALUS_FRONTEND=${daedalus-frontend}

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{Logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    if [ ! -d tls ]; then
      mkdir -p tls/{server,ca}
      ${slimOpenssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout tls/server/server.key -out tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp tls/server/server.crt tls/ca/ca.crt
    fi
    exec ${daedalus-bridge}/bin/cardano-launcher \
      --config ${daedalus-config}/launcher-config.yaml
  '';
in {
   inherit daedalus daedalus-config;
}

