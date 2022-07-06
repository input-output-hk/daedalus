{ system ? builtins.currentSystem
, config ? {}
, nodeImplementation ? "cardano"
, localLib ? import ./lib.nix { inherit nodeImplementation; }
, pkgs ? import (import ./nix/sources.nix).nixpkgs { inherit system config; }
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
, nivOnly ? false
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
      yarn --frozen-lockfile
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
      daedalusPkgs.mock-token-metadata-server
    ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl jq
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
      pkgconfig
      libusb
    ] ++ (localLib.optionals autoStartBackend [
      daedalusPkgs.daedalus-bridge
    ]) ++ (if (pkgs.stdenv.hostPlatform.system == "x86_64-darwin") || (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") then [
      darwin.apple_sdk.frameworks.CoreServices darwin.apple_sdk.frameworks.AppKit
    ] else [
      daedalusPkgs.electron
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
    NODE_EXE = "cardano-wallet";
    CLI_EXE = "cardano-cli";
    NODE_IMPLEMENTATION = nodeImplementation;
    BUILDTYPE = "Debug";
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
      [[ $(type -P cardano-wallet) ]] && source <(cardano-wallet --bash-completion-script cardano-wallet)

      cp -f ${daedalusPkgs.iconPath.small} $DAEDALUS_INSTALL_DIRECTORY/icon.png

      # These links will only occur to binaries that exist for the
      # specific build config
      ln -svf $(type -P cardano-node)
      ln -svf $(type -P cardano-wallet)
      ln -svf $(type -P cardano-cli)
      mkdir -p ${BUILDTYPE}/
      ${let
        # (TODO: investigate why – @michalrus)
        sourceBUILDTYPE = "Release";
      in ''
        ln -svf $PWD/node_modules/usb/build/${sourceBUILDTYPE}/usb_bindings.node ${BUILDTYPE}/
        ln -svf $PWD/node_modules/node-hid/build/${sourceBUILDTYPE}/HID.node ${BUILDTYPE}/
        ln -svf $PWD/node_modules/node-hid/build/${sourceBUILDTYPE}/HID_hidraw.node ${BUILDTYPE}/
        ln -svf $PWD/node_modules/usb-detection/build/${sourceBUILDTYPE}/detection.node ${BUILDTYPE}/
      ''}

      ${pkgs.lib.optionalString (nodeImplementation == "cardano") ''
        source <(cardano-node --bash-completion-script `type -p cardano-node`)
      ''}

      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${daedalusPkgs.nodejs}/include/node -I${toString ./.}/node_modules/node-addon-api"
      ${localLib.optionalString purgeNpmCache ''
        warn "purging all NPM/Yarn caches"
        rm -rf node_modules
        yarn cache clean
        npm cache clean --force
        ''
      }
      yarn install --frozen-lockfile
      yarn build:electron
      ${localLib.optionalString pkgs.stdenv.isLinux ''
        ${pkgs.patchelf}/bin/patchelf --set-rpath ${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc pkgs.udev ]} ${BUILDTYPE}/usb_bindings.node
        ${pkgs.patchelf}/bin/patchelf --set-rpath ${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc pkgs.udev ]} ${BUILDTYPE}/HID.node
        # TODO: is this needed for `detection.node`?
        ${pkgs.patchelf}/bin/patchelf --set-rpath ${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc pkgs.udev ]} ${BUILDTYPE}/detection.node
        ln -svf ${daedalusPkgs.electron}/bin/electron ./node_modules/electron/dist/electron
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
    in if nivOnly then [ niv ] else [ niv daedalusPkgs.cardano-node-cluster.start daedalusPkgs.cardano-node-cluster.stop ];
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
