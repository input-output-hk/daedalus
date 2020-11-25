{ backend ? "cardano"
, network ? "staging"
, os ? "linux"
, jormungandrLib ? (import ../. {}).jormungandrLib
, cardanoLib
, runCommand
, lib
, devShell ? false
, cardano-wallet-native
, runCommandNative
, topologyOverride ? null
, configOverride ? null
, genesisOverride ? null
}:

# Creates an attr set for a cluster containing:
# * launcherConfig (attr set)
# * installerConfig (attr set)
# * nodeConfigFiles
# * configFiles (launcher config + installer config)


let
  clusterOverrides = {
    mainnet_catalyst = {
      cardanoEnv = cardanoLib.environments.mainnet;
      cluster = "mainnet";
      networkName = "mainnet";
    };
    mainnet_flight = {
      cardanoEnv = cardanoLib.environments.mainnet;
      cluster = "mainnet";
      networkName = "mainnet";
    };
    shelley_testnet_v6 = {
      cardanoEnv = cardanoLib.environments.mainnet_candidate_4;
      cluster = "shelley_testnet";
      networkName = "shelley_testnet";
    };
  };
  dirSep = if os == "windows" then "\\" else "/";
  configDir = configFilesSource: {
    linux = configFilesSource;
    macos64 = if devShell then configFilesSource else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
  };

  isDevOrLinux = devShell || os == "linux";

  mkSpacedName = network: "Daedalus ${installDirectorySuffix}";
  spacedName = mkSpacedName network;

  frontendBinPath = let
    frontendBin.linux = "daedalus-frontend";
    frontendBin.windows = "${spacedName}";
    frontendBin.macos64 = "Frontend";
  in frontendBin.${os};


  # Helper function to make a path to a binary
  mkBinPath = binary: let
    binDir = {
      macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}";
      windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
    };
    binary' = if binary == "frontend" then frontendBinPath else binary;
  in if isDevOrLinux then binary' else "${binDir.${os}}${dirSep}${binary'}${lib.optionalString (os == "windows") ".exe"}";
  # Helper function to make a path to a config file
  mkConfigPath = configSrc: configPath: "${(configDir configSrc).${os}}${dirSep}${configPath}";

  envCfg = let
    cardanoEnv = if __hasAttr network clusterOverrides
                 then clusterOverrides.${network}.cardanoEnv
                 else cardanoLib.environments.${network};
    jormungandrEnv = jormungandrLib.environments.${network};
  in if (backend == "cardano") then cardanoEnv else jormungandrEnv;
  kind = if network == "local" then "shelley" else if (envCfg.nodeConfig.Protocol == "RealPBFT" || envCfg.nodeConfig.Protocol == "Byron") then "byron" else "shelley";

  installDirectorySuffix = let
    supportedNetworks = {
      mainnet = "Mainnet";
      mainnet_catalyst = "Catalyst";
      mainnet_flight = "Flight";
      qa = "QA";
      selfnode = "Selfnode";
      local = "Local";
      itn_selfnode = "Selfnode - ITN";
      nightly = "Nightly";
      itn_rewards_v1 = "- Rewards v1";
      staging = "Staging";
      testnet = "Testnet";
      shelley_testnet = "Shelley Testnet";
      shelley_testnet_v6 = "Shelley Testnet v6";
      shelley_qa = "Shelley QA";
    };
    unsupported = "Unsupported";
    networkSupported = __hasAttr network supportedNetworks;
  in if networkSupported then supportedNetworks.${network} else unsupported;

  iconPath = let
    networkIconExists = __pathExists (../. + "/installers/icons/${network}");
    network' = if networkIconExists then network else "mainnet";
  in {
    small = ../installers/icons + "/${network'}/64x64.png";
    large = ../installers/icons + "/${network'}/1024x1024.png";
    base = ../installers/icons + "/${network'}";
  };

  dataDir = let
    path.linux = "\${XDG_DATA_HOME}/Daedalus/${network}";
    path.macos64 = "\${HOME}/Library/Application Support/${spacedName}";
    path.windows = "\${APPDATA}\\${spacedName}";
  in path.${os};

  # Used for flight builds to find legacy paths for migration
  legacyDataDir = let
    path.linux = "\${XDG_DATA_HOME}/Daedalus/mainnet";
    path.macos64 = "\${HOME}/Library/Application Support/Daedalus";
    path.windows = "\${APPDATA}\\Daedalus";
  in path.${os};

  logsPrefix = let
    path.linux = "${dataDir}/Logs";
    path.windows = "Logs";
    path.macos64 = "${dataDir}/Logs";
  in path.${os};

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
    clients = [ {
      organization = "Daedalus";
      commonName = "Daedalus Frontend";
      expiryDays = 365;
    } ];
  };

  launcherLogsPrefix = "${logsPrefix}${dirSep}pub";

  # Default configs for launcher from cardano-shell. Most of these do nothing.
  # TODO: get rid of anything we don't need from cardano-shell
  defaultLauncherConfig = {
    inherit logsPrefix launcherLogsPrefix tlsConfig;
    walletLogging = false;
    daedalusBin = mkBinPath "frontend";
    updateRunnerBin = mkBinPath "update-runner";
    # TODO: set when update system is complete
    updaterArgs = [];
    updaterPath = "";
    updateArchive = "";
    updateWindowsRunner = "";
    workingDir = dataDir;
    stateDir = dataDir;
    tlsPath = "${dataDir}${dirSep}tls";
    cluster = if __hasAttr network clusterOverrides then clusterOverrides.${network}.cluster else network;
    networkName = if __hasAttr network clusterOverrides then clusterOverrides.${network}.networkName else network;
    isCatalyst = network == "mainnet_catalyst";
    isFlight = network == "mainnet_flight";
    isStaging = (envCfg.nodeConfig.RequiresNetworkMagic == "RequiresNoMagic");
    nodeImplementation = backend;
  };

  mkConfigFiles = nodeConfigFiles: launcherConfig: installerConfig:
    runCommand "cfg-files" {
      launcherConfig = builtins.toJSON launcherConfig;
      installerConfig = builtins.toJSON installerConfig;
      passAsFile = [ "launcherConfig" "installerConfig" ];
    } ''
      mkdir $out
      cp ${nodeConfigFiles}/* $out/
      cp $launcherConfigPath $out/launcher-config.yaml
      cp $installerConfigPath $out/installer-config.json
      ${lib.optionalString (envCfg.nodeConfig ? ByronGenesisFile) "cp ${envCfg.nodeConfig.ByronGenesisFile} $out/genesis-byron.json"}
      ${lib.optionalString (envCfg.nodeConfig ? ShelleyGenesisFile) "cp ${envCfg.nodeConfig.ShelleyGenesisFile} $out/genesis-shelley.json"}
    '';

  mkConfigCardano = let
    filterMonitoring = config: if devShell then config else builtins.removeAttrs config [ "hasPrometheus" "hasEKG" ];
    cardanoAddressBin = mkBinPath "cardano-address";
    walletBin = mkBinPath "cardano-wallet";
    nodeBin = mkBinPath "cardano-node";
    cliBin = mkBinPath "cardano-cli";
    nodeConfig = let
      nodeConfigAttrs = if (configOverride == null) then envCfg.nodeConfig else __fromJSON (__readFile configOverride);
    in builtins.toJSON (filterMonitoring (nodeConfigAttrs // (lib.optionalAttrs (!isDevOrLinux || network == "local") {
      ByronGenesisFile = "genesis-byron.json";
      ShelleyGenesisFile = "genesis-shelley.json";
    })));
    genesisFile = let
      genesisFile'.selfnode = ../utils/cardano/selfnode/genesis.json;
      genesisFile'.local = (__fromJSON nodeConfig).GenesisFile;
    in if (genesisOverride != null) then genesisOverride else if (network == "selfnode" || network == "local") then genesisFile'.${network} else envCfg.nodeConfig.ByronGenesisFile;
    normalTopologyFile = if network == "selfnode" then envCfg.topology else cardanoLib.mkEdgeTopology {
      inherit (envCfg) edgePort;
      edgeNodes = [ envCfg.relaysNew ];
    };
    localTopology = cardanoLib.mkEdgeTopology {
      edgePort = 30001;
      edgeNodes = [ "127.0.0.1" ];
    };
    topologyFile = if (topologyOverride == null) then (if network == "local" then localTopology else normalTopologyFile) else topologyOverride;
    nodeConfigFiles = runCommand "node-cfg-files" {
      inherit nodeConfig topologyFile;
      passAsFile = [ "nodeConfig" ];
    } ''
      mkdir $out
      cp ${genesisFile} $out/genesis.json
      cp $nodeConfigPath $out/config.yaml
      cp $topologyFile $out/topology.yaml
      ${lib.optionalString (network == "selfnode") ''
        cp ${envCfg.delegationCertificate} $out/delegation.cert
        cp ${envCfg.signingKey} $out/signing.key
      ''}
    '';

    legacyStateDir = if (network == "mainnet_catalyst") || (network == "mainnet_flight") || (network == "mainnet") then legacyDataDir else dataDir;

    legacyWalletDB = let
      path.linux = "Wallet";
      path.macos64 = "Wallet-1.0";
      path.windows = "Wallet-1.0";
    in path.${os};

    legacySecretKey = let
      path.linux = "Secrets${dirSep}secret.key";
      path.macos64 = "Secrets-1.0${dirSep}secret.key";
      path.windows = "Secrets-1.0${dirSep}secret.key";
    in path.${os};

    launcherConfig = defaultLauncherConfig // {
      inherit
        nodeBin
        cliBin
        walletBin
        cardanoAddressBin
        legacyStateDir
        legacyWalletDB
        legacySecretKey;
      syncTolerance = "300s";
      nodeConfig = {
        inherit kind;
        configurationDir = "";
        network = {
          configFile = mkConfigPath nodeConfigFiles "config.yaml";
          genesisFile = mkConfigPath nodeConfigFiles "genesis.json";
          topologyFile = mkConfigPath nodeConfigFiles "topology.yaml";
        };
        socketFile = if os != "windows" then "${dataDir}${dirSep}cardano-node.socket" else "\\\\.\\pipe\\cardano-node-${network}";
      } // (lib.optionalAttrs (network == "selfnode") {
        delegationCertificate = mkConfigPath nodeConfigFiles "delegation.cert";
        signingKey = mkConfigPath nodeConfigFiles "signing.key";
      });
    } // (lib.optionalAttrs (__hasAttr "smashUrl" envCfg) {
      smashUrl = envCfg.smashUrl;
    });

    installerConfig = {
      installDirectory = if os == "linux" then "Daedalus/${network}" else spacedName;
      inherit spacedName iconPath;
      macPackageName = "Daedalus${network}";
      dataDir = dataDir;
      hasBlock0 = false;
      installerWinBinaries = [
        "cardano-launcher.exe"
        "cardano-node.exe"
        "cardano-wallet.exe"
        "cardano-cli.exe"
        "cardano-address.exe"
      ];
    };

  in {
    inherit nodeConfigFiles launcherConfig installerConfig;
    configFiles = mkConfigFiles nodeConfigFiles launcherConfig installerConfig;
  };

  mkConfigJormungandr = let
    jormungandrConfig = builtins.toJSON (jormungandrLib.mkConfig (envCfg // {
      trustedPeers = envCfg.daedalusPeers or envCfg.trustedPeers;
    }));
    nodeBin = mkBinPath "jormungandr";
    walletBin = mkBinPath "cardano-wallet-jormungandr";
    cliBin = mkBinPath "jcli";
    hasBlock0 = (network == "itn_selfnode") || envCfg ? block0bin;
    nodeConfigFiles = let
    in runCommand "node-cfg-files" {
      buildInputs = [ cardano-wallet-native.jormungandr-cli ];
      jormungandrConfig = if network == "itn_selfnode" then null else jormungandrConfig;
      passAsFile = [ "jormungandrConfig" ];
    } ''
      mkdir $out
      ${if (network == "itn_selfnode") then ''
        cp ${../utils/jormungandr/selfnode/config.yaml} $out/config.yaml
        cp ${../utils/jormungandr/selfnode/secret.yaml} $out/secret.yaml
        cp ${../utils/jormungandr/selfnode/genesis.yaml} $out/genesis.yaml
        jcli genesis encode --input $out/genesis.yaml --output $out/block-0.bin
      '' else ''
        cp $jormungandrConfigPath $out/config.yaml
        ${lib.optionalString hasBlock0 ''
          cp ${envCfg.block0bin} $out/block-0.bin
          jcli genesis hash --input $out/block-0.bin > $out/genesis-hash
        ''}
      ''}
    '';

    secretPath = mkConfigPath nodeConfigFiles "secret.yaml";
    configPath = mkConfigPath nodeConfigFiles "config.yaml";
    block0Path = if hasBlock0 then mkConfigPath nodeConfigFiles "block-0.bin" else "";
    genesisPath = mkConfigPath nodeConfigFiles "genesis.yaml";
    launcherConfig = defaultLauncherConfig // {
      inherit
        nodeBin
        walletBin
        cliBin
        network
        block0Path
        secretPath
        configPath;
      block0Hash = let
        selfnodeHash = builtins.replaceStrings ["\n"] [""] (builtins.readFile (runCommandNative "selfnode-block0.hash" { buildInputs = [ cardano-wallet-native.jormungandr-cli ]; } ''
          jcli genesis hash --input ${nodeConfigFiles}/block-0.bin > $out
        ''));
      in if network == "itn_selfnode" then selfnodeHash else jormungandrLib.environments.${network}.genesisHash;
      syncTolerance = if (network == "itn_selfnode") then "600s" else jormungandrLib.environments.${network}.syncTolerance;
    };
    installerConfig = {
      installDirectory = if os == "linux" then "Daedalus/${network}" else spacedName;
      inherit spacedName iconPath dataDir hasBlock0;
      installerWinBinaries = [ "cardano-launcher.exe" "jormungandr.exe" "cardano-wallet-jormungandr.exe" ];
      macPackageName = "Daedalus${network}";
      configPath = "${nodeConfigFiles}/config.yaml";
    } // (lib.optionalAttrs hasBlock0 {
      block0 = "${nodeConfigFiles}/block-0.bin";
    }) // (lib.optionalAttrs (network == "itn_selfnode") {
      genesisPath = "${nodeConfigFiles}/genesis.yaml";
      secretPath = "${nodeConfigFiles}/secret.yaml";
    });
  in {
    inherit launcherConfig installerConfig nodeConfigFiles;
    configFiles = mkConfigFiles nodeConfigFiles launcherConfig installerConfig;
  };
  configs.jormungandr = mkConfigJormungandr;
  configs.cardano = mkConfigCardano;
in configs.${backend}
