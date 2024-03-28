{ inputs
, targetSystem
}:

let
  internal = inputs.self.internal.${targetSystem};
  inherit (internal) common;
  inherit (common) pkgs;

  devShells = pkgs.lib.genAttrs inputs.self.internal.installerClusters (cluster: let

    launcherConfigs = common.mkLauncherConfigs {
      inherit cluster;
      devShell = true;
    };

    regenerateDevCerts = let
      moddedConfig = pkgs.writeText "launcher-config.yaml" (builtins.toJSON (
        launcherConfigs.launcherConfig
        // {
          daedalusBin = "true";
        }
      ));
    in
      pkgs.writeShellScriptBin "regenerate-dev-certs" ''
        ${common.daedalus-bridge.${cluster}}/bin/cardano-launcher --config ${moddedConfig}
      '';

  in pkgs.stdenv.mkDerivation (rec {
    buildInputs = [
      internal.common.nodejs
      internal.common.yarn
      common.daedalus-bridge.${cluster}
      common.daedalus-installer
      common.mock-token-metadata-server
      regenerateDevCerts
    ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python3 curl jq
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
      pkgconfig
      libusb
    ] ++ (if (pkgs.stdenv.hostPlatform.system == "x86_64-darwin") || (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") then [
      internal.darwin-launcher
      darwin.apple_sdk.frameworks.CoreServices
      darwin.apple_sdk.frameworks.AppKit
      darwin.cctools
      xcbuild
    ] else [
      internal.electronBin
      winePackages.minimal
    ]));
    name = "daedalus";
    buildCommand = "export >$out";
    LAUNCHER_CONFIG = DAEDALUS_CONFIG + "/launcher-config.yaml";
    CARDANO_NODE_VERSION = common.cardanoNodeVersion;
    CARDANO_WALLET_VERSION = common.cardanoWalletVersion;
    DAEDALUS_CONFIG = pkgs.runCommand "daedalus-config" {} ''
      mkdir -pv $out
      cp ${pkgs.writeText "launcher-config.yaml" (builtins.toJSON launcherConfigs.launcherConfig)} $out/launcher-config.yaml
    '';
    DAEDALUS_INSTALL_DIRECTORY = "./";
    DAEDALUS_DIR = DAEDALUS_INSTALL_DIRECTORY;
    CLUSTER = cluster;
    NETWORK = cluster;
    NODE_EXE = "cardano-wallet";
    CLI_EXE = "cardano-cli";
    NODE_IMPLEMENTATION = "cardano";
    BUILDTYPE = "Debug";
    shellHook = let
      secretsDir = if pkgs.stdenv.isLinux then "Secrets" else "Secrets-1.0";
    in ''
      warn() {
         (echo "###"; echo "### WARNING:  $*"; echo "###") >&2
      }

      ${pkgs.lib.optionalString pkgs.stdenv.isLinux "export XDG_DATA_HOME=$HOME/.local/share"}
      ${pkgs.lib.optionalString (cluster == "local") "export CARDANO_NODE_SOCKET_PATH=$(pwd)/state-cluster/bft1.socket"}
      source <(cardano-cli --bash-completion-script cardano-cli)
      source <(cardano-node --bash-completion-script cardano-node)
      source <(cardano-address --bash-completion-script cardano-address)
      [[ $(type -P cardano-wallet) ]] && source <(cardano-wallet --bash-completion-script cardano-wallet)

      cp -f ${launcherConfigs.installerConfig.iconPath.small} $DAEDALUS_INSTALL_DIRECTORY/icon.png

      # These links will only occur to binaries that exist for the
      # specific build config
      ln -svf $(type -P cardano-node)
      ln -svf $(type -P cardano-wallet)
      ln -svf $(type -P cardano-cli)

      source <(cardano-node --bash-completion-script `type -p cardano-node`)

      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${internal.common.nodejs}/include/node -I${toString ../../.}/node_modules/node-addon-api"
      yarn install

      # Rebuild native modules for <https://www.electronjs.org/docs/latest/tutorial/using-native-node-modules>:
      yarn build:electron

      # Patchelf possibly downloaded binary blobs (prebuilds) on Linux (most probably not needed – TODO: check):
      ${pkgs.lib.optionalString pkgs.stdenv.isLinux ''
        for nodeExtension in ${BUILDTYPE}/*.node ; do
          ${pkgs.patchelf}/bin/patchelf --set-rpath ${pkgs.lib.makeLibraryPath [
            pkgs.stdenv.cc.cc pkgs.udev
          ]} "$(readlink -f "$nodeExtension")"
        done
      ''}

      ${pkgs.lib.optionalString pkgs.stdenv.isLinux ''
        # FIXME: use `internal.patchelfElectron`, like Lace
        ln -svf ${internal.electronBin}/bin/electron ./node_modules/electron/dist/electron
      ''}

      echo 'jq < $LAUNCHER_CONFIG'

      echo 'Resolving environment variables to absolute paths…'
      # XXX: they originally contain references to HOME or XDG_DATA_HOME in launcher-config.yaml:
      export CARDANO_WALLET_TLS_PATH="${launcherConfigs.launcherConfig.tlsPath}"

      echo 'Re-generating dev certificates for ‘cardano-wallet’…'
      mkdir -p "$CARDANO_WALLET_TLS_PATH"
      regenerate-dev-certs >/dev/null

      ${common.temporaryNodeModulesPatches}

      echo
      echo 'Now, run ‘yarn dev’.'
    '';
  }));

in devShells // { default = devShells.mainnet; }
