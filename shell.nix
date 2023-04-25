{ system ? builtins.currentSystem
, config ? {}
, nodeImplementation ? "cardano"
, localLib ? import ./lib.nix { inherit nodeImplementation system; }
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
    localLibSystem = system;
    devShell = true;
  };
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
      regenerateDevCerts
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
  regenerateDevCerts = let
    moddedConfig = pkgs.writeText "launcher-config.yaml" (builtins.toJSON (
      daedalusPkgs.launcherConfigs.launcherConfig
      // {
        daedalusBin = "true";
      }
    ));
  in
    pkgs.writeShellScriptBin "regenerate-dev-certs" ''
      ${daedalusPkgs.daedalus-bridge}/bin/cardano-launcher --config ${moddedConfig}
    '';
  gcRoot = pkgs.runCommandLocal "gc-root" {
    properBuildShell = buildShell.overrideAttrs (old: { buildCommand = "export >$out"; });
    cardanoWalletsHaskellNix = daedalusPkgs.walletFlake.outputs.legacyPackages.${system}.roots;
    ourHaskellNix = if pkgs.stdenv.isLinux then daedalusPkgs.yaml2json.project.roots else "";
    daedalusInstallerInputs = with daedalusPkgs.daedalus-installer; buildInputs ++ nativeBuildInputs;
    # cardano-bridge inputs are GC’d, and rebuilt too often on Apple M1 CI:
    cardanoBridgeInputs = builtins.map (attr: if daedalusPkgs ? ${attr} && pkgs.lib.isDerivation daedalusPkgs.${attr} then daedalusPkgs.${attr} else null) (builtins.attrNames (builtins.functionArgs (import ./nix/cardano-bridge.nix)));
  } "export >$out";

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

      # Rebuild native modules for <https://www.electronjs.org/docs/latest/tutorial/using-native-node-modules>:
      find Debug/ Release/ -name '*.node' | xargs rm -v || true
      yarn build:electron

      ${let
        # Several native modules have to be linked in ${BUILDTYPE}/ in
        # root directory, for `yarn dev` to work correctly. If a Debug
        # version of such extension exists, we use it, otherwise, we
        # use Release:
        tryLink = dependency: fileName: ''
          symlinkTarget=$(ls 2>/dev/null -d \
            "$PWD/node_modules/${dependency}/build/Debug/${fileName}" \
            "$PWD/node_modules/${dependency}/build/Release/${fileName}" \
            | head -1
          )

          if [ -z "$symlinkTarget" ] ; then
            echo >&2 "error: symlink target not found: ‘${fileName}’ in ‘${dependency}’"
            # ~exit 1~ — do not exit, let the person fix from inside `nix-shell`
          fi

          ${localLib.optionalString pkgs.stdenv.isLinux ''
            ${pkgs.patchelf}/bin/patchelf --set-rpath ${pkgs.lib.makeLibraryPath [
              pkgs.stdenv.cc.cc pkgs.udev
            ]} "$symlinkTarget"
          ''}

          mkdir -p ${BUILDTYPE}/
          ln -svf "$symlinkTarget" ${BUILDTYPE}/
          unset symlinkTarget
        '';
      in ''
        ${tryLink "usb"           "usb_bindings.node"}
        ${tryLink "usb-detection" "detection.node"}
        ${tryLink "node-hid"      "HID.node"}
        ${localLib.optionalString pkgs.stdenv.isLinux ''
          ${tryLink "node-hid"      "HID_hidraw.node"}
        ''}
      ''}

      ${localLib.optionalString pkgs.stdenv.isLinux ''
        ln -svf ${daedalusPkgs.electron}/bin/electron ./node_modules/electron/dist/electron
      ''}

      echo 'jq < $LAUNCHER_CONFIG'
      echo debug the node by running debug-node

      echo 'Resolving environment variables to absolute paths…'
      # XXX: they originally contain references to HOME or XDG_DATA_HOME in launcher-config.yaml:
      export CARDANO_WALLET_TLS_PATH="${daedalusPkgs.launcherConfigs.launcherConfig.tlsPath}"

      echo 'Re-generating dev certificates for ‘cardano-wallet’…'
      mkdir -p "$CARDANO_WALLET_TLS_PATH"
      regenerate-dev-certs >/dev/null

      echo
      echo 'Now, run ‘yarn dev’.'
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
      inherit (daedalusPkgs.walletFlake.outputs.legacyPackages.${system}.pkgs) niv;
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
in daedalusShell // { inherit fixYarnLock buildShell devops gcRoot; }
