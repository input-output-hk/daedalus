{ system ? builtins.currentSystem
, config ? {}
, nodeImplementation ? "jormungandr"
, localLib ? import ./lib.nix { inherit nodeImplementation; }
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }
, cluster ? "demo"
, systemStart ? null
, autoStartBackend ? systemStart != null
, walletExtraArgs ? []
, allowFaultInjection ? false
, purgeNpmCache ? false
}:

let
  daedalusPkgs = import ./. { inherit cluster; target = system; };
  hostPkgs = import pkgs.path { config = {}; overlays = []; };
  yaml2json = pkgs.haskell.lib.disableCabalFlag pkgs.haskellPackages.yaml "no-exe";
  yarn = pkgs.yarn.override { inherit nodejs; };
  nodejs = pkgs.nodejs-8_x;
  launcher-json = hostPkgs.runCommand "read-launcher-config.json" { buildInputs = [ yaml2json ]; } "yaml2json ${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml > $out";
  fullExtraArgs = walletExtraArgs ++ pkgs.lib.optional allowFaultInjection "--allow-fault-injection";
  patches = builtins.concatLists [
    (pkgs.lib.optional (systemStart != null) ".configuration.systemStart = ${toString systemStart}")
    (pkgs.lib.optional (cluster == "demo") ''.configuration.key = "default"'')
    (pkgs.lib.optional (fullExtraArgs != []) ''.nodeArgs += ${builtins.toJSON fullExtraArgs}'')
    (pkgs.lib.optional (autoStartBackend == true) ''.frontendOnlyMode = true'')
  ];
  patchesString = pkgs.lib.concatStringsSep " | " patches;
  launcherYamlWithStartTime = pkgs.runCommand "launcher-config.yaml" { buildInputs = [ pkgs.jq yaml2json ]; } ''
    jq '${patchesString}' < ${launcher-json} | json2yaml > $out
    echo "Launcher config:  $out"
  '';
  launcherConfig' = if (patches == []) then "${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml" else launcherYamlWithStartTime;
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
  demoTopology = {
    wallet = {
      fallbacks = 7;
      valency = 1;
      relays = [
        [ { addr = "127.0.0.1"; port = 3100; } ]
      ];
    };
  };
  demoTopologyYaml = hostPkgs.runCommand "wallet-topology.yaml" { buildInputs = [ hostPkgs.jq yaml2json ]; } ''
    cat ${builtins.toFile "wallet-topology.json" (builtins.toJSON demoTopology)} | json2yaml > $out
  '';
  demoConfig = pkgs.runCommand "new-config" {} ''
    mkdir $out
    cp ${daedalusPkgs.daedalus.cfg}/etc/* $out/
    rm $out/wallet-topology.yaml
    cp ${demoTopologyYaml} $out/wallet-topology.yaml
  '';
  # This has all the dependencies of daedalusShell, but no shellHook allowing hydra
  # to evaluate it.
  daedalusShellBuildInputs = [
      nodejs yarn
      localLib.cardanoWallet
      localLib.cardanoNode
      localLib.jcli
    ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl jq
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
      chromedriver
    ] ++ (localLib.optionals autoStartBackend [
      daedalusPkgs.daedalus-bridge
    ]) ++ (localLib.optionals (pkgs.stdenv.hostPlatform.system != "x86_64-darwin") [
      daedalusPkgs.electron3
      winePackages.minimal
    ])
    );
  buildShell = pkgs.stdenv.mkDerivation {
    name = "daedalus-build";
    buildInputs = daedalusShellBuildInputs;
  };
  daedalusShell = pkgs.stdenv.mkDerivation (rec {
    buildInputs = daedalusShellBuildInputs;
    name = "daedalus";
    buildCommand = "touch $out";
    LAUNCHER_CONFIG = launcherConfig';
    DAEDALUS_CONFIG = if (cluster == "demo") then demoConfig else "${daedalusPkgs.daedalus.cfg}/etc/";
    DAEDALUS_INSTALL_DIRECTORY = "./";
    DAEDALUS_DIR = DAEDALUS_INSTALL_DIRECTORY;
    CLUSTER = cluster;
    NODE_EXE = if nodeImplementation == "jormungandr" then "cardano-wallet-jormungandr" else "cardano-wallet-http-bridge";
    CLI_EXE = if nodeImplementation == "jormungandr" then "jcli" else "";
    NODE_IMPLEMENTATION = nodeImplementation;
    shellHook = let
      secretsDir = if pkgs.stdenv.isLinux then "Secrets" else "Secrets-1.0";
      systemStartString = builtins.toString systemStart;
    in ''
      warn() {
         (echo "###"; echo "### WARNING:  $*"; echo "###") >&2
      }

      ${localLib.optionalString pkgs.stdenv.isLinux "export XDG_DATA_HOME=$HOME/.local/share"}
      cp -f ${daedalusPkgs.iconPath.${cluster}.small} $DAEDALUS_INSTALL_DIRECTORY/icon.png

      # These links will only occur to binaries that exist for the
      # specific build config
      ln -svf $(type -P jormungandr)
      ln -svf $(type -P cardano-wallet-jormungandr)
      ln -svf $(type -P cardano-http-bridge)
      ln -svf $(type -P cardano-wallet-http-bridge)
      ln -svf $(type -P jcli)

      ${pkgs.lib.optionalString autoStartBackend ''
        for x in wallet-topology.yaml log-config-prod.yaml configuration.yaml mainnet-genesis-dryrun-with-stakeholders.json ; do
          ln -svf ${daedalusPkgs.daedalus.cfg}/etc/$x
        done
        STATE_PATH=$(eval echo $(jq ".statePath" < ${launcher-json}))
        ${pkgs.lib.optionalString (cluster == "demo") ''
          ln -svf ${demoTopologyYaml} wallet-topology.yaml

          # Refresh the old state directory
          rm -rf "''${STATE_PATH}"
          mkdir -p "''${STATE_PATH}"
          echo -n ${systemStartString} > "''${STATE_PATH}/system-start"
        ''}
        mkdir -p "''${STATE_PATH}/${secretsDir}"
      ''}
      ${localLib.optionalString autoStartBackend ''
          TLS_PATH=$(eval echo $(jq ".tlsPath" < ${launcher-json}))
          mkdir -p "''${TLS_PATH}/server" "''${TLS_PATH}/client"
          cardano-x509-certificates \
          --server-out-dir "''${TLS_PATH}/server" \
          --clients-out-dir "''${TLS_PATH}/client" \
          --configuration-file ${daedalusPkgs.daedalus.cfg}/etc/configuration.yaml \
          --configuration-key mainnet_dryrun_full
          echo ''${TLS_PATH}
        ''
      }
      export DAEDALUS_INSTALL_DIRECTORY
      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${nodejs}/include/node"
      ${localLib.optionalString purgeNpmCache ''
        warn "purging all NPM/Yarn caches"
        rm -rf node_modules
        yarn cache clean
        npm cache clean --force
        ''
      }
      yarn install
      ${pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.system != "x86_64-darwin") ''
        ln -svf ${daedalusPkgs.electron3}/bin/electron ./node_modules/electron/dist/electron
        ln -svf ${pkgs.chromedriver}/bin/chromedriver ./node_modules/electron-chromedriver/bin/chromedriver
      ''}
      ${localLib.optionalString (! autoStartBackend) ''
      echo "Instructions for manually running cardano-node:"
      echo "DEPRECATION NOTICE: This should only be used for debugging a specific revision of cardano. Use --autoStartBackend --system-start SYSTEM_START_TIME as parameters to this script to auto-start the wallet"
      echo "In cardano repo run scripts/launch/demo-nix.sh -w"
      echo "export CARDANO_TLS_PATH=/path/to/cardano-sl/state-demo/tls/wallet"
      echo "yarn dev"
      ''}
    '';
  });
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
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = let
      inherit (localLib.iohkNix) niv;
      inherit (localLib) cardanoWallet jormungandr jcli;
    in [ niv cardanoWallet jormungandr jcli ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };
in daedalusShell // { inherit fixYarnLock buildShell devops; }
