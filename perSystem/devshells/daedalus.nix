{inputs, ...}: {
  perSystem = {
    config,
    system,
    lib,
    inputs',
    pkgs,
    common,
    linuxBuild ? null,
    darwinBuild ? null,
    ...
  }: let
    # Platform-specific build for devshell extras (relocatableElectron on Linux, darwin-launcher on Darwin)
    platformBuild =
      if pkgs.stdenv.hostPlatform.isLinux
      then linuxBuild
      else if pkgs.stdenv.hostPlatform.isDarwin
      then darwinBuild
      else null;

    installerClusters = common.sourceLib.installerClusters;

    rustToolchain = with inputs'.fenix.packages;
      combine [
        stable.rustc
        stable.cargo
        stable.clippy
        stable.rustfmt
      ];

    mkDaedalusShell = cluster: let
      daedalusConfigs = common.mkDaedalusConfigs {
        inherit cluster;
        devShell = true;
      };
      regenerateDevCerts = let
        moddedConfig = pkgs.writeText "launcher-config.yaml" (builtins.toJSON (
          daedalusConfigs.daedalusConfig // {daedalusBin = "true";}
        ));
      in
        pkgs.writeShellScriptBin "regenerate-dev-certs" ''
          ${common.daedalus-bridge.${cluster}}/bin/cardano-launcher --config ${moddedConfig}
        '';
    in
      pkgs.stdenv.mkDerivation rec {
        name = "daedalus";
        buildInputs = with pkgs;
          [
            nix
            rustToolchain
            common.nodejs
            common.yarn
            common.daedalus-bridge.${cluster}
            regenerateDevCerts
            bash
            binutils
            coreutils
            curl
            gnutar
            git
            python3
            jq
            nodePackages.node-gyp
            nodePackages.node-pre-gyp
            gnumake
            pkg-config
            libusb1
            shellcheck
            config.treefmt.build.wrapper
          ]
          ++ (
            if pkgs.stdenv.hostPlatform.isDarwin
            then [
              platformBuild.darwin-launcher
              pkgs.darwin.cctools
              pkgs.xcbuild
              pkgs.perl
            ]
            else [
              platformBuild.relocatableElectron
              pkgs.winePackages.minimal
            ]
          );
        buildCommand = "export >$out";
        CARDANO_NODE_VERSION = common.cardanoNodeVersion;
        CARDANO_WALLET_VERSION = common.cardanoWalletVersion;
        DAEDALUS_CLUSTER = daedalusConfigs.daedalusConfig.cluster;
        DAEDALUS_NETWORK_NAME = daedalusConfigs.daedalusConfig.networkName;
        DAEDALUS_IS_FLIGHT =
          if daedalusConfigs.daedalusConfig.isFlight
          then "true"
          else "false";
        DAEDALUS_UPDATE_MODE = "system-package-disabled";
        DAEDALUS_INSTALL_DIRECTORY = "./";
        DAEDALUS_DIR = DAEDALUS_INSTALL_DIRECTORY;
        CLUSTER = cluster;
        NETWORK = cluster;
        NODE_EXE = "cardano-wallet";
        CLI_EXE = "cardano-cli";
        NODE_IMPLEMENTATION = "cardano";
        BUILDTYPE = "Debug";
        shellHook = ''
          warn() {
             (echo "###"; echo "### WARNING:  $*"; echo "###") >&2
          }

          ${lib.optionalString pkgs.stdenv.isLinux ''
            export XDG_DATA_HOME=$HOME/.local/share
            # Prevent GTK from loading system ibus/input method modules that require newer glibc
            export GTK_IM_MODULE=xim
            export GTK_PATH_SUFFIX=
            export GTK_EXE_PREFIX=
          ''}
          ${lib.optionalString (cluster == "local") "export CARDANO_NODE_SOCKET_PATH=$(pwd)/state-cluster/bft1.socket"}
          source <(cardano-cli --bash-completion-script cardano-cli)
          source <(cardano-node --bash-completion-script cardano-node)
          source <(cardano-address --bash-completion-script cardano-address)
          [[ $(type -P cardano-wallet) ]] && source <(cardano-wallet --bash-completion-script cardano-wallet)

          cp -f ${daedalusConfigs.installerConfig.iconPath.small} $DAEDALUS_INSTALL_DIRECTORY/icon.png

          ln -svf $(type -P cardano-node)
          ln -svf $(type -P cardano-wallet)
          ln -svf $(type -P cardano-cli)

          source <(cardano-node --bash-completion-script `type -p cardano-node`)

          export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${common.nodejs}/include/node -I${toString ../../.}/node_modules/node-addon-api"
          yarn install

          # Rebuild native modules for <https://www.electronjs.org/docs/latest/tutorial/using-native-node-modules>:
          yarn build:electron

          # Patchelf possibly downloaded binary blobs (prebuilds) on Linux (most probably not needed – TODO: check):
          ${lib.optionalString pkgs.stdenv.isLinux ''
            for nodeExtension in ${BUILDTYPE}/*.node ; do
              ${pkgs.patchelf}/bin/patchelf --set-rpath ${lib.makeLibraryPath [
              pkgs.stdenv.cc.cc
              pkgs.systemd
              pkgs.libusb1
            ]} "$(readlink -f "$nodeExtension")"
            done
          ''}

          ${lib.optionalString pkgs.stdenv.isLinux ''
            ln -svf ${platformBuild.relocatableElectron}/bin/electron ./node_modules/electron/dist/electron
          ''}

          export DAEDALUS_STATE_DIR="${daedalusConfigs.daedalusConfig.stateDir}"
          export DAEDALUS_LOGS_DIR="${daedalusConfigs.daedalusConfig.logsPrefix}"
          export DAEDALUS_LEGACY_STATE_DIR="${daedalusConfigs.daedalusConfig.legacyStateDir}"
          export CARDANO_WALLET_TLS_PATH="${daedalusConfigs.daedalusConfig.tlsPath}"

          echo 'Re-generating dev certificates for cardano-wallet…'
          mkdir -p "$CARDANO_WALLET_TLS_PATH"
          regenerate-dev-certs >/dev/null

          ${common.temporaryNodeModulesPatches}

          echo
          echo 'Now, run yarn dev.'
        '';
      };

    daedalusShells = pkgs.lib.genAttrs installerClusters mkDaedalusShell;
  in {
    devShells = daedalusShells // {default = daedalusShells.mainnet;};
    # Exposed as a package so `nix run .#patch-electron-rebuild` works from
    # rebuild-native-modules.sh without going through self.internal.
    packages.patch-electron-rebuild = common.patchElectronRebuild;
  };
}
