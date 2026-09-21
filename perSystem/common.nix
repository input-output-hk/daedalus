# Inlines: lib/source-lib.nix, lib/cardano-bridge.nix, lib/daedalus-config.nix, lib/common.nix
# Provides _module.args.common and _module.args.mkCommon for all perSystem modules.
{inputs, ...}: {
  perSystem = {
    pkgs,
    system,
    lib,
    ...
  }: let
    # ---------------------------------------------------------------------------
    # Inlined source-lib.nix
    # ---------------------------------------------------------------------------
    mkSourceLib = let
      readClustersFile = fileName: let
        unique = builtins.foldl' (acc: e:
          if builtins.elem e acc
          then acc
          else acc ++ [e]) [];
      in
        unique (
          builtins.map builtins.unsafeDiscardStringContext (
            builtins.filter (el: builtins.isString el && el != "") (
              builtins.split "[ \n\r\t]+" (
                builtins.readFile fileName
              )
            )
          )
        );
    in rec {
      installerClusters = readClustersFile (inputs.self + "/installer-clusters.cfg");

      forEach = xs: fun:
        builtins.listToAttrs
        (builtins.map (cluster: {
            name = cluster;
            value = fun cluster;
          })
          xs);

      forEachCluster = forEach installerClusters;

      # When did the project start? → `git show --no-patch --date=unix $(git rev-list --max-parents=0 HEAD)`
      daedalusEpoch = 1475675335;

      buildRev =
        if inputs.self ? shortRev
        then inputs.self.rev
        else "0000000000000000000000000000000000000000";
      buildRevShort =
        if inputs.self ? shortRev
        then builtins.substring 0 9 buildRev
        else if inputs.self ? dirtyShortRev
        then inputs.self.dirtyShortRev
        else "dirty";
      buildCounter =
        if inputs.self ? rev && inputs.self ? lastModified
        then (inputs.self.lastModified - daedalusEpoch) / (60 * 60)
        else 0;
    };

    # ---------------------------------------------------------------------------
    # mkCommon factory: takes a targetSystem string, returns the common attrset.
    # pkgs is always the host (build) pkgs; targetSystem controls which binaries
    # are selected from the upstream flakes.
    # ---------------------------------------------------------------------------
    mkCommon = targetSystem: let
      sourceLib = mkSourceLib;

      flakeLock = builtins.fromJSON (builtins.readFile (inputs.self + "/flake.lock"));

      flake-compat = import inputs.flake-compat;

      walletFlake =
        (flake-compat {
          src = inputs.cardano-wallet;
        }).defaultNix;

      nodeFlake =
        (flake-compat {
          src = inputs.cardano-node;
        }).defaultNix;

      walletPackages =
        {
          x86_64-windows = walletFlake.packages.x86_64-linux.windowsPackages;
          x86_64-linux = walletFlake.packages.x86_64-linux.staticPackages;
          x86_64-darwin = walletFlake.packages.x86_64-darwin;
          aarch64-darwin = walletFlake.packages.aarch64-darwin;
        }.${
          targetSystem
        };

      nodePackages =
        {
          x86_64-windows = nodeFlake.legacyPackages.x86_64-linux.hydraJobs.windows; # a bug in ${cardano-node}/flake.nix
          x86_64-linux = nodeFlake.hydraJobs.x86_64-linux.musl;
          x86_64-darwin = nodeFlake.packages.x86_64-darwin;
          aarch64-darwin = nodeFlake.packages.aarch64-darwin;
        }.${
          targetSystem
        };

      mithrilPackages = {
        x86_64-windows = inputs.mithril.packages.x86_64-linux.mithril-client-cli-windows;
        x86_64-linux = inputs.mithril.packages.x86_64-linux.mithril-client-cli;
        x86_64-darwin = inputs.mithril.packages.x86_64-darwin.mithril-client-cli;
        aarch64-darwin = inputs.mithril.packages.aarch64-darwin.mithril-client-cli;
      };

      # cardano-watchdog: locally-built Rust process supervisor.
      cardano-watchdog = let
        cargoLockExists = builtins.pathExists ../watchdog/Cargo.lock;
        isWindows = targetSystem == "x86_64-windows";
        isLinux = pkgs.stdenv.hostPlatform.isLinux;
        src = pkgs.lib.fileset.toSource {
          root = ../watchdog;
          fileset = pkgs.lib.fileset.unions [
            ../watchdog/Cargo.toml
            ../watchdog/Cargo.lock
            ../watchdog/src
          ];
        };
        fenixPkgs = inputs.fenix.packages.${pkgs.stdenv.hostPlatform.system};
      in
        if !cargoLockExists
        then null
        else if isWindows
        then
          # Cross-compile to x86_64-pc-windows-gnu (mingw) from Linux, same as mithril-client.
          let
            mingwToolchain = fenixPkgs.combine [
              fenixPkgs.stable.cargo
              fenixPkgs.stable.rustc
              fenixPkgs.targets.x86_64-pc-windows-gnu.stable.rust-std
            ];
            craneLib = (inputs.crane.mkLib pkgs).overrideToolchain mingwToolchain;
            mingwW64 = pkgs.pkgsCross.mingwW64;
          in
            craneLib.buildPackage {
              inherit src;
              pname = "cardano-watchdog";
              version = "0.1.0";
              strictDeps = true;
              CARGO_BUILD_TARGET = "x86_64-pc-windows-gnu";
              CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER = "${mingwW64.stdenv.cc}/bin/${mingwW64.stdenv.cc.targetPrefix}cc";
              CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUSTFLAGS = "-L ${mingwW64.windows.pthreads}/lib";
              CC_x86_64_pc_windows_gnu = "${mingwW64.stdenv.cc}/bin/${mingwW64.stdenv.cc.targetPrefix}cc";
              NASM = "${pkgs.nasm}/bin/nasm";
              depsBuildBuild = [mingwW64.stdenv.cc pkgs.nasm];
              cargoExtraArgs = "--bin cardano-watchdog";
              doCheck = false;
            }
        else if isLinux
        then
          # Build a fully static musl binary so nix-bundle-exe doesn't need to trace
          # and bundle glibc, libgcc_s, etc.
          let
            muslToolchain = fenixPkgs.combine [
              fenixPkgs.stable.cargo
              fenixPkgs.stable.rustc
              fenixPkgs.targets.x86_64-unknown-linux-musl.stable.rust-std
            ];
            craneLib = (inputs.crane.mkLib pkgs).overrideToolchain muslToolchain;
          in
            craneLib.buildPackage {
              inherit src;
              pname = "cardano-watchdog";
              version = "0.1.0";
              strictDeps = true;
              CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
              CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER = "${pkgs.pkgsStatic.stdenv.cc}/bin/${pkgs.pkgsStatic.stdenv.cc.targetPrefix}cc";
              cargoExtraArgs = "--bin cardano-watchdog";
            }
        else let
          toolchain = fenixPkgs.stable.toolchain;
          craneLib = (inputs.crane.mkLib pkgs).overrideToolchain toolchain;
        in
          craneLib.buildPackage {
            inherit src;
            pname = "cardano-watchdog";
            version = "0.1.0";
            strictDeps = true;
            cargoExtraArgs = "--bin cardano-watchdog";
          };

      inherit (walletFlake.legacyPackages.${pkgs.stdenv.hostPlatform.system}.pkgs) cardanoLib;

      inherit (walletPackages) cardano-wallet cardano-address;
      inherit (nodePackages) cardano-node cardano-cli snapshot-converter;
      mithril-client = mithrilPackages.${targetSystem};

      cardano-shell =
        (flake-compat {
          src = inputs.cardano-shell;
        }).defaultNix;

      cardano-launcher = cardano-shell.hydraJobs.cardano-launcher.${targetSystem};

      # ---------------------------------------------------------------------------
      # Inlined cardano-bridge.nix as a function
      # ---------------------------------------------------------------------------
      mkDaedalusBridge = {target ? targetSystem, ...}:
        pkgs.runCommandCC "daedalus-cardano-bridge" {
          passthru = {
            node-version = cardano-node.passthru.identifier.version;
            wallet-version = cardano-wallet.version;
          };
        } ''
          mkdir -pv $out/bin
          cd $out/bin
          echo ${cardano-wallet.version} > $out/version
          copy_glob() {
            local label="$1"
            local pattern="$2"
            set -- $pattern
            if [ "$1" != "$pattern" ]; then
              cp -f "$@" . || true
            else
              echo "WARNING: Missing binaries for ''${label} (''${pattern})"
            fi
          }
          copy_glob "cardano-wallet" "${cardano-wallet}/bin/*"
          copy_glob "cardano-address" "${cardano-address}/bin/cardano-address*"
          copy_glob "cardano-launcher" "${cardano-launcher}/bin/*"
          copy_glob "cardano-node" "${cardano-node}/bin/*"
          copy_glob "cardano-cli" "${cardano-cli}/bin/cardano-cli*"
          copy_glob "snapshot-converter" "${snapshot-converter}/bin/snapshot-converter*"
          copy_glob "mithril-client" "${mithril-client}/bin/mithril-client*"
          ${lib.optionalString (cardano-watchdog != null) ''
            copy_glob "cardano-watchdog" "${cardano-watchdog}/bin/cardano-watchdog*"
          ''}
          ${lib.optionalString (target == "x86_64-windows") ''
            # Upstream mithril package can emit a binary named `mithril-client`
            # even for Windows cross builds; NSIS/runtime expect `mithril-client.exe`.
            if [ -f mithril-client ] && [ ! -f mithril-client.exe ]; then
              cp -f mithril-client mithril-client.exe
            fi
          ''}
          ${lib.optionalString (target == "aarch64-darwin") ''
            chmod +w -R .
            for x in cardano-address cardano-node cardano-launcher cardano-cli cardano-wallet mithril-client snapshot-converter; do
              ${pkgs.darwin.sigtool}/bin/codesign --force -s - $x
            done
          ''}
        '';

      daedalus-bridge =
        pkgs.lib.genAttrs sourceLib.installerClusters (_cluster:
          mkDaedalusBridge {});

      cardanoNodeVersion = cardano-node.identifier.version + "-" + builtins.substring 0 9 nodeFlake.rev;
      cardanoWalletVersion = daedalus-bridge.mainnet.wallet-version + "-" + builtins.substring 0 9 walletFlake.rev;

      # ---------------------------------------------------------------------------
      # Inlined daedalus-config.nix as a function
      # ---------------------------------------------------------------------------
      mkDaedalusConfigsImpl = {
        devShell ? false,
        cluster,
      }: let
        network = cluster;
        os =
          {
            x86_64-windows = "windows";
            x86_64-linux = "linux";
            x86_64-darwin = "macos64";
            aarch64-darwin = "macos64-arm";
          }.${
            targetSystem
          };

        clustersAvailable = rec {
          mainnet = fromCardanoPlayground "mainnet";
          mainnet_flight = mainnet;
          shelley_qa = fromCardanoPlayground "shelley_qa";
          vasil_dev = fromCardanoPlayground "vasil-dev";
          preprod = fromCardanoPlayground "preprod";
          preview = fromCardanoPlayground "preview";
        };

        smashServers = {
          mainnet = "https://smash.cardano-mainnet.iohk.io";
          preprod = "https://preprod-smash.world.dev.cardano.org";
          preview = "https://preview-smash.world.dev.cardano.org";
        };

        tokenMetadataServers = {
          mainnet = "https://tokens.cardano.org";
          preprod = "https://metadata.world.dev.cardano.org";
          preview = "https://metadata.world.dev.cardano.org";
        };

        fromCardanoPlayground = envName: let
          originalFiles = builtins.path {
            name = "cardano-playground-config-${envName}";
            path = inputs.cardano-playground + ("/docs/environments-pre/" + envName);
          };

          originalNodeConfig = builtins.fromJSON (builtins.unsafeDiscardStringContext (
            builtins.readFile (originalFiles + "/config.json")
          ));

          nodeConfig =
            originalNodeConfig
            // {
              AlonzoGenesisFile = originalFiles + "/" + originalNodeConfig.AlonzoGenesisFile;
              ByronGenesisFile = originalFiles + "/" + originalNodeConfig.ByronGenesisFile;
              ShelleyGenesisFile = originalFiles + "/" + originalNodeConfig.ShelleyGenesisFile;
              minSeverity = "Info"; # XXX: Needed for sync % updates.
            }
            // (
              if originalNodeConfig ? ConwayGenesisFile
              then {
                ConwayGenesisFile = originalFiles + "/" + originalNodeConfig.ConwayGenesisFile;
              }
              else {}
            );
        in {
          cluster = envName;
          networkName = envName;
          cardanoEnv =
            {
              inherit nodeConfig;
              topologyFile = originalFiles + "/topology.json";
              peerSnapshotFile = originalFiles + "/peer-snapshot.json";
              metadataUrl = tokenMetadataServers.${envName};
            }
            // (let
              checkpointsFile = originalFiles + "/checkpoints.json";
            in
              if builtins.pathExists checkpointsFile
              then {inherit checkpointsFile;}
              else {});
        };

        dirSep =
          if os == "windows"
          then "\\"
          else "/";
        configDir = configFilesSource: {
          linux =
            if devShell
            then configFilesSource
            else "\${ENTRYPOINT_DIR}/config";
          macos64 =
            if devShell
            then configFilesSource
            else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
          macos64-arm =
            if devShell
            then configFilesSource
            else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
          windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
        };

        mkSpacedName = network': "Daedalus ${installDirectorySuffix}";
        spacedName = mkSpacedName network;

        frontendBinPath = let
          frontendBin.linux = "daedalus-frontend";
          frontendBin.windows = "${spacedName}";
          frontendBin.macos64 = "Frontend";
          frontendBin.macos64-arm = "Frontend";
        in
          frontendBin.${os};

        mkBinPath = binary: let
          binDir = {
            macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}";
            macos64-arm = "\${DAEDALUS_INSTALL_DIRECTORY}";
            windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
          };
          binary' =
            if binary == "frontend"
            then frontendBinPath
            else binary;
        in
          if (devShell || os == "linux")
          then binary'
          else "${binDir.${os}}${dirSep}${binary'}${lib.optionalString (os == "windows") ".exe"}";

        mkConfigPath = configSrc: configPath: "${(configDir configSrc).${os}}${dirSep}${configPath}";

        envCfg = clustersAvailable.${network}.cardanoEnv;
        kind =
          if network == "local"
          then "shelley"
          else if (envCfg.nodeConfig.Protocol == "RealPBFT" || envCfg.nodeConfig.Protocol == "Byron")
          then "byron"
          else "shelley";

        installDirectorySuffix = let
          supportedNetworks = {
            mainnet = "Mainnet";
            mainnet_flight = "Flight";
            local = "Local";
            staging = "Staging";
            testnet = "Testnet";
            shelley_qa = "Shelley QA";
            alonzo_purple = "Alonzo Purple";
            vasil_dev = "Vasil-Dev";
            preprod = "Pre-Prod";
            preview = "Preview";
          };
          unsupported = "Unsupported";
          networkSupported = __hasAttr network supportedNetworks;
        in
          if networkSupported
          then supportedNetworks.${network}
          else unsupported;

        iconPath = let
          networkIconExists = __pathExists (../installers/icons + "/${network}");
          network' =
            if networkIconExists
            then network
            else "mainnet";
        in {
          small = ../installers/icons + "/${network'}/64x64.png";
          large = ../installers/icons + "/${network'}/1024x1024.png";
          base = ../installers/icons + "/${network'}";
        };

        dataDir = let
          path.linux = "\${XDG_DATA_HOME}/Daedalus/${network}";
          path.macos64 = "\${HOME}/Library/Application Support/${spacedName}";
          path.macos64-arm = "\${HOME}/Library/Application Support/${spacedName}";
          path.windows = "\${APPDATA}\\${spacedName}";
        in
          path.${os};

        legacyDataDir = let
          path.linux = "\${XDG_DATA_HOME}/Daedalus/mainnet";
          path.macos64 = "\${HOME}/Library/Application Support/Daedalus";
          path.macos64-arm = "\${HOME}/Library/Application Support/Daedalus";
          path.windows = "\${APPDATA}\\Daedalus";
        in
          path.${os};

        logsPrefix = let
          path.linux = "${dataDir}/Logs";
          path.windows = "${dataDir}${dirSep}Logs";
          path.macos64 = "${dataDir}/Logs";
          path.macos64-arm = "${dataDir}/Logs";
        in
          path.${os};

        tlsConfig = {
          ca = {
            organization = "Daedalus";
            commonName = "Daedalus Self-Signed Root CA";
            expiryDays = 3650;
          };
          server = {
            organization = "Daedalus";
            commonName = "Daedalus Wallet Backend";
            expiryDays = 365;
            altDNS = [
              "localhost"
              "localhost.localdomain"
              "127.0.0.1"
              "::1"
            ];
          };
          clients = [
            {
              organization = "Daedalus";
              commonName = "Daedalus Frontend";
              expiryDays = 365;
            }
          ];
        };

        launcherLogsPrefix = "${logsPrefix}${dirSep}pub";

        defaultDaedalusConfig = {
          inherit logsPrefix launcherLogsPrefix tlsConfig;
          walletLogging = false;
          daedalusBin = mkBinPath "frontend";
          updateRunnerBin = mkBinPath "update-runner";
          updaterArgs = [];
          updaterPath = "";
          updateArchive = "";
          updateWindowsRunner = "";
          workingDir = dataDir;
          stateDir = dataDir;
          tlsPath = "${dataDir}${dirSep}tls";
          cluster =
            if __hasAttr network clustersAvailable
            then clustersAvailable.${network}.cluster
            else network;
          networkName =
            if __hasAttr network clustersAvailable
            then clustersAvailable.${network}.networkName
            else network;
          isFlight = network == "mainnet_flight";
          isStaging = envCfg.nodeConfig.RequiresNetworkMagic == "RequiresNoMagic";
          nodeImplementation = "cardano";
        };

        mkConfigFiles = nodeConfigFiles: daedalusConfig: installerConfig: let
          isLinux = os == "linux";
          isWindows = os == "windows";

          wStateDir =
            if isLinux
            then "\${DAEDALUS_DIR}/${network}"
            else dataDir;

          wSocketPath =
            if isWindows
            then "\\\\.\\pipe\\cardano-node-${network}"
            else "${wStateDir}/cardano-node.socket";

          wChainPath = "${wStateDir}${dirSep}chain";
          wWalletDbPath = "${wStateDir}${dirSep}wallets";

          wNodeBin =
            if isLinux
            then "\${ENTRYPOINT_DIR}/libexec/cardano-node"
            else mkBinPath "cardano-node";
          wWalletBin =
            if isLinux
            then "\${ENTRYPOINT_DIR}/libexec/cardano-wallet"
            else mkBinPath "cardano-wallet";
          wMithrilBin =
            if isLinux
            then "\${ENTRYPOINT_DIR}/libexec/mithril-client"
            else mkBinPath "mithril-client";
          wSnapshotConverterBin =
            if isLinux
            then "\${ENTRYPOINT_DIR}/libexec/snapshot-converter"
            else mkBinPath "snapshot-converter";

          wConfigYaml =
            if isLinux
            then "\${ENTRYPOINT_DIR}/config/config.yaml"
            else mkConfigPath nodeConfigFiles "config.yaml";
          wTopologyYaml =
            if isLinux
            then "\${ENTRYPOINT_DIR}/config/topology.yaml"
            else mkConfigPath nodeConfigFiles "topology.yaml";
          wByronGenesisPath = mkConfigPath nodeConfigFiles "genesis-byron.json";
          wNetworkMagicArgs =
            if (envCfg.nodeConfig.RequiresNetworkMagic or "RequiresMagic") == "RequiresNoMagic"
            then ["--mainnet"]
            else ["--testnet" wByronGenesisPath];

          wElectronBin =
            if isLinux
            then "\${ENTRYPOINT_DIR}/libexec/daedalus-frontend"
            else mkBinPath frontendBinPath;

          wElectronArgs = [];

          daedalusConfigJson =
            {
              node = {
                exe = wNodeBin;
                args = [
                  "run"
                  "--config"
                  wConfigYaml
                  "--topology"
                  wTopologyYaml
                  "--database-path"
                  wChainPath
                  "--socket-path"
                  wSocketPath
                ];
                state_dir = wStateDir;
                socket_path = wSocketPath;
              };
              wallet = {
                exe = wWalletBin;
                args =
                  [
                    "serve"
                    "--node-socket"
                    wSocketPath
                    "--database"
                    wWalletDbPath
                    "--sync-tolerance"
                    "300s"
                  ]
                  ++ wNetworkMagicArgs;
                state_dir = wStateDir;
              };
              electron = {
                exe = wElectronBin;
                args = wElectronArgs;
                env =
                  {
                    DAEDALUS_STATE_DIR = wStateDir;
                    DAEDALUS_LOGS_DIR = logsPrefix;
                    DAEDALUS_CLUSTER = daedalusConfig.cluster;
                    DAEDALUS_NETWORK_NAME = daedalusConfig.networkName;
                    DAEDALUS_LEGACY_STATE_DIR = daedalusConfig.legacyStateDir;
                    DAEDALUS_IS_FLIGHT =
                      if daedalusConfig.isFlight
                      then "true"
                      else "false";
                    DAEDALUS_UPDATE_MODE =
                      if isLinux
                      then "system-package-disabled"
                      else "installer-managed";
                  }
                  // lib.optionalAttrs (!isLinux) {
                    DAEDALUS_UPDATE_RUNNER = daedalusConfig.updateRunnerBin;
                  }
                  // lib.optionalAttrs (daedalusConfig ? smashUrl) {
                    DAEDALUS_SMASH_URL = daedalusConfig.smashUrl;
                  };
              };
            }
            // (lib.optionalAttrs (!isLinux) {
              pub_logs_dir = "${wStateDir}${dirSep}Logs${dirSep}pub";
              tls_dir = "${wStateDir}${dirSep}tls";
            })
            // (lib.optionalAttrs (daedalusConfig ? mithrilAggregatorUrl) {
              mithril = {
                mithril_bin = wMithrilBin;
                snapshot_converter_bin = wSnapshotConverterBin;
                converter_config = wConfigYaml;
                aggregator_url = daedalusConfig.mithrilAggregatorUrl;
                genesis_vkey = daedalusConfig.mithrilGenesisVkey;
                ancillary_vkey = daedalusConfig.mithrilAncillaryVkey;
                state_dir = wStateDir;
                chain_path = wChainPath;
              };
            });
        in
          pkgs.runCommand "cfg-files" {
            installerConfig = builtins.toJSON installerConfig;
            daedalusConfigJson = builtins.toJSON daedalusConfigJson;
            passAsFile = ["installerConfig" "daedalusConfigJson"];
          } ''
            mkdir $out
            cp ${nodeConfigFiles}/* $out/
            cp $installerConfigPath $out/installer-config.json
            cp $daedalusConfigJsonPath $out/daedalus-config.json
            ${lib.optionalString (envCfg.nodeConfig ? ByronGenesisFile) "cp ${envCfg.nodeConfig.ByronGenesisFile} $out/genesis-byron.json"}
            ${lib.optionalString (envCfg.nodeConfig ? ShelleyGenesisFile) "cp ${envCfg.nodeConfig.ShelleyGenesisFile} $out/genesis-shelley.json"}
            ${lib.optionalString (envCfg.nodeConfig ? AlonzoGenesisFile) "cp ${envCfg.nodeConfig.AlonzoGenesisFile} $out/genesis-alonzo.json"}
            ${lib.optionalString (envCfg.nodeConfig ? ConwayGenesisFile) "cp ${envCfg.nodeConfig.ConwayGenesisFile} $out/genesis-conway.json"}
            # Ensure ledger is configured with lsm backend and relative path
            chmod u+w $out/config.yaml
            ${lib.getExe pkgs.jq} '.LedgerDB = {Backend: "V2LSM", LSMDatabasePath: "lsm/"} | .ConsensusMode = "PraosMode"' < ${nodeConfigFiles}/config.yaml > $out/config.yaml
          '';

        mkConfigCardano = let
          filterMonitoring = config:
            if devShell
            then config
            else builtins.removeAttrs config ["hasPrometheus" "hasEKG"];
          cardanoAddressBin = mkBinPath "cardano-address";
          walletBin = mkBinPath "cardano-wallet";
          nodeBin = mkBinPath "cardano-node";
          cliBin = mkBinPath "cardano-cli";
          watchdogBin = mkBinPath "cardano-watchdog";
          mithrilBin = mkBinPath "mithril-client";
          snapshotConverterBin = mkBinPath "snapshot-converter";
          mithrilNetworkCfgs = let
            readVkey = networkDir: name:
              lib.removeSuffix "\n" (builtins.readFile
                (inputs.mithril + "/mithril-infra/configuration/${networkDir}/${name}.vkey"));
            mkNetworkCfg = networkDir: aggregatorUrl: {
              inherit aggregatorUrl;
              genesisVkey = readVkey networkDir "genesis";
              ancillaryVkey = readVkey networkDir "ancillary";
            };
          in rec {
            mainnet =
              mkNetworkCfg "release-mainnet"
              "https://aggregator.release-mainnet.api.mithril.network/aggregator";
            mainnet_flight = mainnet;
            preprod =
              mkNetworkCfg "release-preprod"
              "https://aggregator.release-preprod.api.mithril.network/aggregator";
            preview =
              mkNetworkCfg "pre-release-preview"
              "https://aggregator.pre-release-preview.api.mithril.network/aggregator";
          };
          nodeConfig = let
            nodeConfigAttrs =
              if (null == null)
              then envCfg.nodeConfig
              else __fromJSON (__readFile null);
          in
            builtins.toJSON (filterMonitoring (nodeConfigAttrs
              // (lib.optionalAttrs (!devShell || network == "local") ({
                  ByronGenesisFile = "genesis-byron.json";
                  ShelleyGenesisFile = "genesis-shelley.json";
                  AlonzoGenesisFile = "genesis-alonzo.json";
                }
                // (
                  if nodeConfigAttrs ? ConwayGenesisFile
                  then {
                    ConwayGenesisFile = "genesis-conway.json";
                  }
                  else {}
                )))));
          genesisFile = envCfg.nodeConfig.ByronGenesisFile;
          topologyFile =
            if envCfg ? topologyFile
            then envCfg.topologyFile
            else throw "no topologyFile in envCfg";
          nodeConfigFiles =
            pkgs.runCommand "node-cfg-files" {
              inherit nodeConfig topologyFile;
              passAsFile = ["nodeConfig"];
            } ''
              mkdir $out
              cp ${genesisFile} $out/genesis.json
              cp $nodeConfigPath $out/config.yaml
              cp $topologyFile $out/topology.yaml
              ${lib.optionalString (envCfg ? peerSnapshotFile) ''
                cp ${envCfg.peerSnapshotFile} $out/peer-snapshot.json
              ''}
              ${lib.optionalString (envCfg ? checkpointsFile) ''
                cp ${envCfg.checkpointsFile} $out/checkpoints.json
              ''}
            '';

          legacyStateDir =
            if (network == "mainnet_flight") || (network == "mainnet")
            then legacyDataDir
            else dataDir;

          legacyWalletDB = let
            path.linux = "Wallet";
            path.macos64 = "Wallet-1.0";
            path.macos64-arm = "Wallet-1.0";
            path.windows = "Wallet-1.0";
          in
            path.${os};

          legacySecretKey = let
            path.linux = "Secrets${dirSep}secret.key";
            path.macos64 = "Secrets-1.0${dirSep}secret.key";
            path.macos64-arm = "Secrets-1.0${dirSep}secret.key";
            path.windows = "Secrets-1.0${dirSep}secret.key";
          in
            path.${os};

          daedalusConfig =
            defaultDaedalusConfig
            // {
              inherit
                nodeBin
                cliBin
                walletBin
                watchdogBin
                cardanoAddressBin
                legacyStateDir
                legacyWalletDB
                legacySecretKey
                ;
              wipeChain = false;
              mithrilPartialSyncEnabled = true;
              mithrilPartialSyncThresholdImmutables = 20;
              syncTolerance = "300s";
              nodeConfig = {
                inherit kind;
                configurationDir = "";
                network = {
                  configFile = mkConfigPath nodeConfigFiles "config.yaml";
                  genesisFile = mkConfigPath nodeConfigFiles "genesis.json";
                  topologyFile = mkConfigPath nodeConfigFiles "topology.yaml";
                };
              };
            }
            // (lib.optionalAttrs (envCfg ? metadataUrl) {
              metadataUrl = envCfg.metadataUrl;
            })
            // (lib.optionalAttrs (__hasAttr network smashServers) {
              smashUrl = smashServers.${network};
            })
            // (lib.optionalAttrs (mithrilNetworkCfgs ? ${network}) {
              inherit mithrilBin snapshotConverterBin;
              mithrilAggregatorUrl = mithrilNetworkCfgs.${network}.aggregatorUrl;
              mithrilGenesisVkey = mithrilNetworkCfgs.${network}.genesisVkey;
              mithrilAncillaryVkey = mithrilNetworkCfgs.${network}.ancillaryVkey;
              mithrilConverterConfig = mkConfigPath nodeConfigFiles "config.yaml";
            });

          installerConfig = {
            installDirectory =
              if os == "linux"
              then "Daedalus/${network}"
              else spacedName;
            inherit spacedName iconPath;
            uglyName = "daedalus";
            macPackageName = "Daedalus${network}";
            dataDir = dataDir;
            installerWinBinaries = [
              "cardano-watchdog.exe"
              "cardano-node.exe"
              "cardano-wallet.exe"
              "cardano-cli.exe"
              "cardano-address.exe"
            ];
          };
        in {
          inherit nodeConfigFiles daedalusConfig installerConfig;
          configFiles = mkConfigFiles nodeConfigFiles daedalusConfig installerConfig;
        };
      in
        mkConfigCardano;

      mkDaedalusConfigs = {
        devShell ? false,
        cluster,
      }:
        mkDaedalusConfigsImpl {inherit devShell cluster;};

      daedalusConfigs = pkgs.lib.genAttrs sourceLib.installerClusters (cluster:
        mkDaedalusConfigs {
          devShell = false;
          inherit cluster;
        });

      originalPackageJson = builtins.fromJSON (builtins.readFile ../package.json);

      nodejs = let
        base = pkgs.nodejs_24;
      in
        if !(pkgs.lib.hasInfix "-darwin" targetSystem)
        then base
        else
          base.overrideAttrs (drv: {
            patches = pkgs.lib.filter (patch:
              !(
                pkgs.lib.hasInfix "bypass-xcodebuild" patch
                || pkgs.lib.hasInfix "bypass-darwin-xcrun" patch
              ))
            drv.patches;
          });

      yarn = (pkgs.yarn.override {inherit nodejs;}).overrideAttrs (drv: {
        postFixup =
          (drv.postFixup or "")
          + ''
            sed -r 's,#!/bin/sh,#!${pkgs.bash}/bin/sh,g' -i $out/libexec/yarn/lib/cli.js
          '';
      });

      yarn2nix = import "${inputs.nixpkgs}/pkgs/development/tools/yarn2nix-moretea" {
        inherit pkgs;
        inherit nodejs yarn;
        allowAliases = true;
      };

      srcLockfiles = pkgs.lib.cleanSourceWith {
        src = inputs.self;
        name = "daedalus-lockfiles";
        filter = name: _type: let b = baseNameOf (toString name); in (b == "package.json" || b == "yarn.lock");
      };

      srcWithoutNix = pkgs.lib.cleanSourceWith {
        src = inputs.self;
        filter = name: type: let
          baseName = baseNameOf (toString name);
          relPath = pkgs.lib.removePrefix (toString inputs.self + "/") (toString name);
        in
          !(type
            == "regular"
            && (
              pkgs.lib.hasInfix "-source/nix/" name
              || pkgs.lib.hasSuffix ".nix" name
              || pkgs.lib.hasSuffix ".hs" name
              || pkgs.lib.hasSuffix ".cabal" name
            ))
          && !(type
            == "directory"
            && (
              baseName
              == ".direnv"
              || baseName == ".agent"
              || baseName == "node_modules"
              || baseName == "dist"
              || baseName == "release"
              || baseName == ".git"
              || baseName == "watchdog"
            ))
          && !(baseName == ".claude" || baseName == ".opencode")
          && !(baseName == ".envrc")
          && !(pkgs.lib.hasSuffix ".md" name && type == "regular" && !(pkgs.lib.hasInfix "/terms-of-use/" name));
      };

      offlineCache = yarn2nix.importOfflineCache (yarn2nix.mkYarnNix {
        yarnLock = srcLockfiles + "/yarn.lock";
      });

      nodegypInstallVersion =
        (pkgs.lib.importJSON "${nodejs}/lib/node_modules/npm/node_modules/node-gyp/package.json").installVersion;

      setupCacheAndGypDirs = ''
        # XXX: `HOME` (for various caches) cannot be under our source root, that confuses `electron-packager`:
        export HOME=$(realpath $NIX_BUILD_TOP/home)
        mkdir -p $HOME

        # Do not look up in the registry, but in the offline cache, cf. <https://classic.yarnpkg.com/en/docs/yarnrc>:
        echo '"--offline" true' >>$HOME/.yarnrc
        echo '"--frozen-lockfile" true' >>$HOME/.yarnrc
        yarn config set yarn-offline-mirror ${offlineCache}

        # Don't try to download prebuilded packages (with prebuild-install):
        export npm_config_build_from_source=true
        ( echo 'buildFromSource=true' ; echo 'compile=true' ; ) >$HOME/.prebuild-installrc

        # Skip electron binary download in install scripts (we use pkgs.electron.unwrapped instead):
        export ELECTRON_SKIP_BINARY_DOWNLOAD=1

        ${pkgs.lib.concatMapStringsSep "\n" (cacheDir: ''

            # Node.js headers for building native `*.node` extensions with node-gyp:
            mkdir -p ${cacheDir}/node-gyp/${nodejs.version}
            echo ${toString nodegypInstallVersion} > ${cacheDir}/node-gyp/${nodejs.version}/installVersion
            ln -sf ${nodejs}/include ${cacheDir}/node-gyp/${nodejs.version}

          '') [
            "$HOME/.cache" # Linux, Windows (cross-compiled)
            "$HOME/Library/Caches" # Darwin
          ]}

        mkdir -p $HOME/.electron-gyp/
        ln -sf ${commonSources.electronHeaders} $HOME/.electron-gyp/${electronVersion}

        # These are sometimes useful:
        #
        # npm config set loglevel verbose
        # echo '"--verbose" true' >>$HOME/.yarnrc
        # export NODE_OPTIONS='--trace-warnings'
        # export DEBUG='*'
        # export DEBUG='node-gyp @electron/get:* electron-rebuild'
      '';

      temporaryNodeModulesPatches = ''
        sed -r "s/'127\.0\.0\.1'/undefined/g" -i node_modules/cardano-launcher/dist/src/cardanoNode.js

        # Has to be idempotent:
        if ! grep -qF "'-N'" node_modules/cardano-launcher/dist/src/cardanoWallet.js ; then
          sed -r "s/'serve'/\0, '+RTS', '-N', '-RTS'/g" -i node_modules/cardano-launcher/dist/src/cardanoWallet.js
        fi

        # Has to be idempotent:
        if ! grep -qF "'-N'" node_modules/cardano-launcher/dist/src/cardanoNode.js ; then
          sed -r "s/config.rtsOpts/(\0 || []).concat(['-N'])/g" -i node_modules/cardano-launcher/dist/src/cardanoNode.js
        fi
      '';

      electronVersion = originalPackageJson.dependencies.electron;
      electronChromedriverVersion = electronVersion;

      commonSources = let
        electronHeadersTarball = pkgs.runCommand "electron-headers-${electronVersion}.tar.gz" {
          nativeBuildInputs = [pkgs.gnutar];
        } "tar czf $out -C ${pkgs.electron.headers} .";
      in {
        electronHeaders = pkgs.runCommandLocal "electron-headers" {src = electronHeadersTarball;} ''
          cp -r ${pkgs.electron.headers}/. $out
          chmod -R +w $out
          echo ${toString nodegypInstallVersion} >$out/installVersion
        '';

        electronShaSums = pkgs.fetchurl {
          name = "electronShaSums-${electronVersion}";
          url = "https://github.com/electron/electron/releases/download/v${electronVersion}/SHASUMS256.txt";
          hash = "sha256-+tI8kWgYS9VrI+DRiXkhN0Nt1CT3yAWxcw8N72XrUE8=";
        };

        electronCacheHash =
          builtins.hashString "sha256"
          "https://github.com/electron/electron/releases/download/v${electronVersion}";

        electronChromedriverShaSums = pkgs.fetchurl {
          name = "electronChromedriverShaSums-${electronChromedriverVersion}";
          url = "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}/SHASUMS256.txt";
          hash = "sha256-+tI8kWgYS9VrI+DRiXkhN0Nt1CT3yAWxcw8N72XrUE8=";
        };

        electronChromedriverCacheHash =
          builtins.hashString "sha256"
          "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}";
      };

      patchElectronRebuild = pkgs.writeShellScriptBin "patch-electron-rebuild" ''
        echo 'Patching electron-rebuild to force our Node.js headers and CXXFLAGS…'

        tarball="''${1:-${commonSources.electronHeaders.src}}"
        nodedir="''${2:-${commonSources.electronHeaders}}"

        echo "  → tarball=$tarball"
        echo "  → nodedir=$nodedir"
        echo "  → forcing CXXFLAGS=-Wno-error for Darwin builds"

        nodeGypJs="node_modules/@electron/rebuild/lib/module-type/node-gyp/node-gyp.js"

        # Patch idempotently (matters in repetitive shell.nix):
        if ! grep -qF "$tarball" $nodeGypJs ; then
          sed -r "s|const extraNodeGypArgs.*|\0 extraNodeGypArgs.push('--tarball', '$tarball', '--nodedir', '$nodedir'); process.env.CXXFLAGS='-Wno-error'; process.env.npm_config_cxxflags='-Wno-error';|" -i $nodeGypJs
        fi

        echo "  → result=$(grep -F "const extraNodeGypArgs" $nodeGypJs)"
      '';
    in {
      inherit
        sourceLib
        flakeLock
        flake-compat
        walletFlake
        nodeFlake
        walletPackages
        nodePackages
        mithrilPackages
        cardano-watchdog
        cardanoLib
        cardano-wallet
        cardano-address
        cardano-node
        cardano-cli
        snapshot-converter
        mithril-client
        cardano-shell
        cardano-launcher
        daedalus-bridge
        cardanoNodeVersion
        cardanoWalletVersion
        mkDaedalusConfigs
        daedalusConfigs
        originalPackageJson
        nodejs
        yarn
        yarn2nix
        srcLockfiles
        srcWithoutNix
        offlineCache
        nodegypInstallVersion
        setupCacheAndGypDirs
        temporaryNodeModulesPatches
        electronVersion
        electronChromedriverVersion
        commonSources
        patchElectronRebuild
        ;
    };

    common = mkCommon system;
  in {
    _module.args.mkCommon = mkCommon;
    _module.args.common = common;
  };
}
