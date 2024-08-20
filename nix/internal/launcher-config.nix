{ network ? "staging"
, os ? "linux"
, cardanoLib
, runCommand
, lib
, devShell ? false
, topologyOverride ? null
, configOverride ? null
, genesisOverride ? null
, cardano-playground
, system
}:

# Creates an attr set for a cluster containing:
# * launcherConfig (attr set)
# * installerConfig (attr set)
# * nodeConfigFiles
# * configFiles (launcher config + installer config)


let
  clustersAvailable = rec {
    mainnet    = fromCardanoPlayground "mainnet";
    mainnet_flight = mainnet;
    shelley_qa = fromCardanoPlayground "shelley_qa";
    vasil_dev  = fromCardanoPlayground "vasil-dev";
    preprod    = fromCardanoPlayground "preprod";
    preview    = fromCardanoPlayground "preview";
  };

  smashServers = {
    mainnet = "https://smash.cardano-mainnet.iohk.io";
    preprod = "https://preprod-smash.world.dev.cardano.org";
    preview = "https://preview-smash.world.dev.cardano.org";
  };

  fromCardanoPlayground = envName: let
    originalFiles = builtins.path {
      name = "cardano-playground-config-${envName}";
      path = cardano-playground + ("/static/book.play.dev.cardano.org/environments/" + envName);
    };

    originalNodeConfig = builtins.fromJSON (builtins.unsafeDiscardStringContext (
      builtins.readFile (originalFiles + "/config.json")));

    nodeConfig = originalNodeConfig // {
      AlonzoGenesisFile  = originalFiles + "/" + originalNodeConfig.AlonzoGenesisFile;
      ByronGenesisFile   = originalFiles + "/" + originalNodeConfig.ByronGenesisFile;
      ShelleyGenesisFile = originalFiles + "/" + originalNodeConfig.ShelleyGenesisFile;
      minSeverity = "Info";  # XXX: Needed for sync % updates.
    } // (if originalNodeConfig ? ConwayGenesisFile then {
      ConwayGenesisFile  = originalFiles + "/" + originalNodeConfig.ConwayGenesisFile;
    } else {});
  in {
    cluster = envName;
    networkName = envName;
    cardanoEnv = {
      inherit nodeConfig;
      topologyFile = originalFiles + "/topology.json";
    };
  };

  dirSep = if os == "windows" then "\\" else "/";
  configDir = configFilesSource: {
    linux = if devShell then configFilesSource else "\${ENTRYPOINT_DIR}/config";
    macos64 = if devShell then configFilesSource else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
    macos64-arm = if devShell then configFilesSource else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
  };

  mkSpacedName = network: "Daedalus ${installDirectorySuffix}";
  spacedName = mkSpacedName network;

  frontendBinPath = let
    frontendBin.linux = "daedalus-frontend";
    frontendBin.windows = "${spacedName}";
    frontendBin.macos64 = "Frontend";
    frontendBin.macos64-arm = "Frontend";
  in frontendBin.${os};

  selfnodeConfig = rec {
    useByronWallet = true;
    private = false;
    networkConfig = import ./selfnode-config.nix;
    nodeConfig = networkConfig // cardanoLib.defaultLogConfig;
    consensusProtocol = networkConfig.Protocol;
    genesisFile = ../../utils/cardano/selfnode/genesis.json;
    delegationCertificate = ../../utils/cardano/selfnode/selfnode.cert;
    signingKey = ../../utils/cardano/selfnode/selfnode.key;
    topology = ../../utils/cardano/selfnode/selfnode-topology.json;
  };

  # Helper function to make a path to a binary
  mkBinPath = binary: let
    binDir = {
      macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}";
      macos64-arm = "\${DAEDALUS_INSTALL_DIRECTORY}";
      windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
    };
    binary' = if binary == "frontend" then frontendBinPath else binary;
  in if (devShell || os == "linux") then binary' else "${binDir.${os}}${dirSep}${binary'}${lib.optionalString (os == "windows") ".exe"}";
  # Helper function to make a path to a config file
  mkConfigPath = configSrc: configPath: "${(configDir configSrc).${os}}${dirSep}${configPath}";

  envCfg = let
    cardanoEnv = clustersAvailable.${network}.cardanoEnv;
  in if network == "selfnode" then selfnodeConfig else cardanoEnv;
  kind = if network == "local" then "shelley" else if (envCfg.nodeConfig.Protocol == "RealPBFT" || envCfg.nodeConfig.Protocol == "Byron") then "byron" else "shelley";

  installDirectorySuffix = let
    supportedNetworks = {
      mainnet = "Mainnet";
      mainnet_flight = "Flight";
      selfnode = "Selfnode";
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
  in if networkSupported then supportedNetworks.${network} else unsupported;

  iconPath = let
    networkIconExists = __pathExists (../../. + "/installers/icons/${network}");
    network' = if networkIconExists then network else "mainnet";
  in {
    small = ../../installers/icons + "/${network'}/64x64.png";
    large = ../../installers/icons + "/${network'}/1024x1024.png";
    base = ../../installers/icons + "/${network'}";
  };

  dataDir = let
    path.linux = "\${XDG_DATA_HOME}/Daedalus/${network}";
    path.macos64 = "\${HOME}/Library/Application Support/${spacedName}";
    path.macos64-arm = "\${HOME}/Library/Application Support/${spacedName}";
    path.windows = "\${APPDATA}\\${spacedName}";
  in path.${os};

  # Used for flight builds to find legacy paths for migration
  legacyDataDir = let
    path.linux = "\${XDG_DATA_HOME}/Daedalus/mainnet";
    path.macos64 = "\${HOME}/Library/Application Support/Daedalus";
    path.macos64-arm = "\${HOME}/Library/Application Support/Daedalus";
    path.windows = "\${APPDATA}\\Daedalus";
  in path.${os};

  logsPrefix = let
    path.linux = "${dataDir}/Logs";
    path.windows = "Logs";
    path.macos64 = "${dataDir}/Logs";
    path.macos64-arm = "${dataDir}/Logs";
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
    cluster = if __hasAttr network clustersAvailable then clustersAvailable.${network}.cluster else network;
    networkName = if __hasAttr network clustersAvailable then clustersAvailable.${network}.networkName else network;
    isFlight = network == "mainnet_flight";
    isStaging = (envCfg.nodeConfig.RequiresNetworkMagic == "RequiresNoMagic");
    nodeImplementation = "cardano";
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
      ${lib.optionalString (envCfg.nodeConfig ? AlonzoGenesisFile) "cp ${envCfg.nodeConfig.AlonzoGenesisFile} $out/genesis-alonzo.json"}
      ${lib.optionalString (envCfg.nodeConfig ? ConwayGenesisFile) "cp ${envCfg.nodeConfig.ConwayGenesisFile} $out/genesis-conway.json"}
    '';

  mkConfigCardano = let
    filterMonitoring = config: if devShell then config else builtins.removeAttrs config [ "hasPrometheus" "hasEKG" ];
    cardanoAddressBin = mkBinPath "cardano-address";
    walletBin = mkBinPath "cardano-wallet";
    nodeBin = mkBinPath "cardano-node";
    cliBin = mkBinPath "cardano-cli";
    nodeConfig = let
      nodeConfigAttrs = if (configOverride == null) then envCfg.nodeConfig else __fromJSON (__readFile configOverride);
    in builtins.toJSON (filterMonitoring (nodeConfigAttrs // (lib.optionalAttrs (!devShell || network == "local") ({
      ByronGenesisFile = "genesis-byron.json";
      ShelleyGenesisFile = "genesis-shelley.json";
      AlonzoGenesisFile = "genesis-alonzo.json";
    } // (if nodeConfigAttrs ? ConwayGenesisFile then {
      ConwayGenesisFile = "genesis-conway.json";
    } else {})))));
    genesisFile = let
      genesisFile'.selfnode = ../../utils/cardano/selfnode/genesis.json;
      genesisFile'.local = (__fromJSON nodeConfig).GenesisFile;
    in if (genesisOverride != null) then genesisOverride else if (network == "selfnode" || network == "local") then genesisFile'.${network} else envCfg.nodeConfig.ByronGenesisFile;
    normalTopologyFile = if network == "selfnode" then envCfg.topology else throw "no longer supported";
    localTopology = cardanoLib.mkEdgeTopology {
      edgePort = 30001;
      edgeNodes = [ "127.0.0.1" ];
    };
    topologyFile =
      if envCfg ? topologyFile
      then envCfg.topologyFile
      else if (topologyOverride == null)
           then (if network == "local"
                 then localTopology
                 else normalTopologyFile)
           else topologyOverride;
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

    legacyStateDir = if (network == "mainnet_flight") || (network == "mainnet") then legacyDataDir else dataDir;

    legacyWalletDB = let
      path.linux = "Wallet";
      path.macos64 = "Wallet-1.0";
      path.macos64-arm = "Wallet-1.0";
      path.windows = "Wallet-1.0";
    in path.${os};

    legacySecretKey = let
      path.linux = "Secrets${dirSep}secret.key";
      path.macos64 = "Secrets-1.0${dirSep}secret.key";
      path.macos64-arm = "Secrets-1.0${dirSep}secret.key";
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
      };
    } // (lib.optionalAttrs (network == "selfnode") {
      selfnodeBin = mkBinPath "local-cluster";
      mockTokenMetadataServerBin = mkBinPath "mock-token-metadata-server";
    }) // (lib.optionalAttrs (__hasAttr network smashServers) {
      smashUrl = smashServers.${network};
    }) // (lib.optionalAttrs (__hasAttr "metadataUrl" envCfg) {
      metadataUrl = envCfg.metadataUrl;
    });

    installerConfig = {
      installDirectory = if os == "linux" then "Daedalus/${network}" else spacedName;
      inherit spacedName iconPath;
      uglyName = "daedalus";
      macPackageName = "Daedalus${network}";
      dataDir = dataDir;
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

in mkConfigCardano
