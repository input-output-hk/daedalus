{ system ? builtins.currentSystem
, config ? {}
, nodeImplementation ? "cardano"
, localLib ? import ./lib.nix { inherit nodeImplementation; }
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }
, cluster ? "selfnode"
, systemStart ? null
, autoStartBackend ? systemStart != null
, walletExtraArgs ? []
, allowFaultInjection ? false
, purgeNpmCache ? false
, topologyOverride ? null
, configOverride ? null
, genesisOverride ? null
, useLocalNode ? false
}:

let
  daedalusPkgs = import ./. {
    inherit nodeImplementation cluster topologyOverride configOverride genesisOverride useLocalNode;
    target = system;
    devShell = true;
  };
  hostPkgs = import pkgs.path { config = {}; overlays = []; };
  fullExtraArgs = walletExtraArgs ++ pkgs.lib.optional allowFaultInjection "--allow-fault-injection";
  launcherConfig' = "${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml";
  fixYarnLock = pkgs.stdenv.mkDerivation {
    name = "fix-yarn-lock";
    buildInputs = [ daedalusPkgs.nodejs daedalusPkgs.yarn pkgs.git ];
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
  # This has all the dependencies of daedalusShell, but no shellHook allowing hydra
  # to evaluate it.
  daedalusShellBuildInputs = [
      daedalusPkgs.nodejs
      daedalusPkgs.yarn
      daedalusPkgs.daedalus-bridge
      daedalusPkgs.daedalus-installer
      daedalusPkgs.darwin-launcher
    ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl jq
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
      chromedriver
    ] ++ (localLib.optionals autoStartBackend [
      daedalusPkgs.daedalus-bridge
    ]) ++ (if (pkgs.stdenv.hostPlatform.system == "x86_64-darwin") then [
      darwin.apple_sdk.frameworks.CoreServices
    ] else [
      daedalusPkgs.electron8
      winePackages.minimal
    ])
    ) ++ (pkgs.lib.optionals (nodeImplementation == "cardano") [
      debug.node
    ]);
  buildShell = pkgs.stdenv.mkDerivation {
    name = "daedalus-build";
    buildInputs = daedalusShellBuildInputs;
  };
  debug.node = pkgs.writeShellScriptBin "debug-node" (with daedalusPkgs.launcherConfigs.launcherConfig; ''
    cardano-node run --topology ${nodeConfig.network.topologyFile} --config ${nodeConfig.network.configFile} --database-path ${stateDir}/chain --port 3001 --socket-path ${stateDir}/cardano-node.socket
  '');
  daedalusShell = pkgs.stdenv.mkDerivation (rec {
    buildInputs = daedalusShellBuildInputs;
    name = "daedalus";
    buildCommand = "touch $out";
    LAUNCHER_CONFIG = launcherConfig';
    DAEDALUS_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/";
    DAEDALUS_INSTALL_DIRECTORY = "./";
    DAEDALUS_DIR = DAEDALUS_INSTALL_DIRECTORY;
    CLUSTER = cluster;
    NODE_EXE = if nodeImplementation == "jormungandr" then "cardano-wallet-jormungandr" else "cardano-wallet-http-bridge";
    CLI_EXE = if nodeImplementation == "jormungandr" then "jcli" else "";
    NODE_IMPLEMENTATION = nodeImplementation;
    shellHook = let
      secretsDir = if pkgs.stdenv.isLinux then "Secrets" else "Secrets-1.0";
    in ''
      warn() {
         (echo "###"; echo "### WARNING:  $*"; echo "###") >&2
      }

      ${localLib.optionalString pkgs.stdenv.isLinux "export XDG_DATA_HOME=$HOME/.local/share"}
      ${localLib.optionalString (cluster == "local") "export CARDANO_NODE_SOCKET_PATH=$(pwd)/state-cluster/bft1.socket"}
      source <(cardano-cli --bash-completion-script cardano-cli)
      source <(cardano-node --bash-completion-script cardano-node)
      source <(cardano-address --bash-completion-script cardano-address)
      [[ $(type -P cardano-wallet-shelley) ]] && source <(cardano-wallet-shelley --bash-completion-script cardano-wallet-shelley)
      [[ $(type -P cardano-wallet-byron) ]] && source <(cardano-wallet-byron --bash-completion-script cardano-wallet-byron)

      cp -f ${daedalusPkgs.iconPath.small} $DAEDALUS_INSTALL_DIRECTORY/icon.png

      # These links will only occur to binaries that exist for the
      # specific build config
      ln -svf $(type -P jormungandr)
      ln -svf $(type -P cardano-wallet-jormungandr)
      ln -svf $(type -P jcli)
      ${pkgs.lib.optionalString (nodeImplementation == "cardano") ''
        source <(cardano-node --bash-completion-script `type -p cardano-node`)
      ''}

      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${daedalusPkgs.nodejs}/include/node"
      ${localLib.optionalString purgeNpmCache ''
        warn "purging all NPM/Yarn caches"
        rm -rf node_modules
        yarn cache clean
        npm cache clean --force
        ''
      }
      yarn install
      ${pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.system != "x86_64-darwin") ''
        ln -svf ${daedalusPkgs.electron8}/bin/electron ./node_modules/electron/dist/electron
        ln -svf ${pkgs.chromedriver}/bin/chromedriver ./node_modules/electron-chromedriver/bin/chromedriver
      ''}
      echo 'jq < $LAUNCHER_CONFIG'
      echo debug the node by running debug-node
    '';
  });
  daedalus = daedalusShell.overrideAttrs (oldAttrs: {
    shellHook = ''
      ${oldAttrs.shellHook}
      yarn dev
      exit 0
    '';
  });
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = let
      inherit (localLib.iohkNix) niv;
    in [ niv daedalusPkgs.cardano-node-cluster.start daedalusPkgs.cardano-node-cluster.stop ];
    shellHook = ''
      export CARDANO_NODE_SOCKET_PATH=$(pwd)/state-cluster/bft1.socket
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * start-cluster - start a development cluster
        * stop-cluster - stop a development cluster

      "
    '';
  };
in daedalusShell // { inherit fixYarnLock buildShell devops; }
