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
}:

# Creates an attr set for a cluster containing:
# * launcherConfig (attr set)
# * installerConfig (attr set)
# * nodeConfigFiles
# * configFiles (launcher config + installer config)


let
  dirSep = if os == "windows" then "\\" else "/";
  configDir = configFilesSource: {
    linux = configFilesSource;
    macos64 = if devShell then configFilesSource else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
  };

  baseName = if network == "mainnet" then "Daedalus" else "Daedalus ${installDirectorySuffix.${network}}";
  spacedName = if backend == "cardano" then "${baseName} Flight" else baseName;

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
  in if (devShell || os == "linux") then binary' else "${binDir.${os}}${dirSep}${binary'}${lib.optionalString (os == "windows") ".exe"}";
  # Helper function to make a path to a config file
  mkConfigPath = configSrc: configPath: "${(configDir configSrc).${os}}${dirSep}${configPath}";

  envCfg = (if (backend == "cardano") then cardanoLib else jormungandrLib).environments.${network};

  installDirectorySuffix = {
    qa = "QA";
    selfnode = "Selfnode";
    itn_selfnode = "Selfnode - ITN";
    nightly = "Nightly";
    itn_rewards_v1 = "- Rewards v1";
    staging = "Staging";
    testnet = "Testnet";
  };


  dataDir = let
    path.linux = "\${XDG_DATA_HOME}/Daedalus/${network}";
    path.macos64 = "\${HOME}/Library/Application Support/${baseName}";
    path.windows = "\${APPDATA}\\${baseName}";
  in path.${os};

  logsPrefix = let
    suffix = if backend == "cardano" then "ByronReboot" else "";
    path.linux = "${dataDir}/Logs";
    path.windows = "Logs";
    path.macos64 = "${dataDir}/Logs";
  in "${path.${os}}${suffix}";

  launcherLogsPrefix = "${logsPrefix}${dirSep}pub";

  # Default configs for launcher from cardano-shell. Most of these do nothing.
  # TODO: get rid of anything we don't need from cardano-shell
  defaultLauncherConfig = {
    inherit logsPrefix launcherLogsPrefix;
    walletLogging = false;
    nodeArgs = [];
    daedalusBin = mkBinPath "frontend";
    nodeLogPath = null;
    updaterArgs = [];
    updaterPath = "";
    updateArchive = "";
    updateWindowsRunner = "";
    workingDir = dataDir;
    stateDir = dataDir;
    x509ToolPath = null;
    frontendOnlyMode = true;
    tlsPath = null;
    cluster = network;
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
    '';

  mkConfigByron = let
    exportWalletsBin = mkBinPath "export-wallets";
    dbConverterBin = mkBinPath "db-converter";
    walletBin = mkBinPath "cardano-wallet-byron";
    nodeBin = mkBinPath "cardano-node";
    cliBin = mkBinPath "cardano-cli";
    nodeConfig = builtins.toJSON envCfg.nodeConfig;
    genesisFile = if (network == "selfnode") then ../utils/cardano/selfnode/genesis.json else envCfg.genesisFile;
    topologyFile = if network == "selfnode" then envCfg.topology else cardanoLib.mkEdgeTopology {
      inherit (envCfg) edgePort;
      edgeNodes = [ envCfg.relaysNew ];
    };
    nodeConfigFiles = let
      genesisFile = if (network == "selfnode") then ../utils/cardano/selfnode/genesis.json else envCfg.genesisFile;
    in runCommand "node-cfg-files" {
      inherit nodeConfig;
      topologyFile = if network == "selfnode" then envCfg.topology else cardanoLib.mkEdgeTopology {
        inherit (envCfg) edgePort;
        edgeNodes = [ envCfg.relaysNew ];
      };
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
    legacyWalletDB = let
      path.linux = "${dataDir}${dirSep}Wallet";
      path.macos64 = "${dataDir}${dirSep}Wallet-1.0";
      path.windows = "${dataDir}${dirSep}Wallet-1.0";
    in path.${os};
    legacySecretKey = let
      path.linux = "${dataDir}${dirSep}Secrets${dirSep}secret.key";
      path.macos64 = "${dataDir}${dirSep}Secrets-1.0${dirSep}secret.key";
      path.windows = "${dataDir}${dirSep}Secrets-1.0${dirSep}secret.key";
    in path.${os};
    launcherConfig = defaultLauncherConfig // {
      inherit
        nodeBin
        cliBin
        walletBin
        exportWalletsBin
        dbConverterBin
        legacyWalletDB
        legacySecretKey;
      networkName = network;
      syncTolerance = "300s";
      nodeConfig = {
        kind = "byron";
        configurationDir = "";
        network = {
          configFile = mkConfigPath nodeConfigFiles "config.yaml";
          genesisFile = mkConfigPath nodeConfigFiles "genesis.json";
          genesisHash = if (network != "selfnode") then envCfg.genesisHash else "";
          topologyFile = mkConfigPath nodeConfigFiles "topology.yaml";
        };
        socketFile = if os != "windows" then "${dataDir}${dirSep}cardano-node.socket" else "\\\\.\\pipe\\cardano-node-${network}";
      } // (lib.optionalAttrs (network == "selfnode") {
        delegationCertificate = mkConfigPath nodeConfigFiles "delegation.cert";
        signingKey = mkConfigPath nodeConfigFiles "signing.key";
      });
    };

    installerConfig = {
      installDirectory = if os == "linux" then "Daedalus/${network}" else spacedName;
      inherit spacedName;
      macPackageName = "Daedalus${network}";
      dataDir = dataDir;
      hasBlock0 = false;
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
      inherit spacedName;
      macPackageName = "Daedalus${network}";
      inherit dataDir;
      inherit hasBlock0;
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
  configs.cardano = mkConfigByron;
in configs.${backend}
