{ backend ? "cardano"
, environment ? "staging"
, os ? "linux"
, jormungandrLib ? (import ../. {}).jormungandrLib
, cardanoLib
, runCommand
, lib
, devShell ? false
, cardano-wallet-native
, runCommandNative
}:
let
  dirSep = if os == "windows" then "\\" else "/";
  envCfg = (if (backend == "cardano") then cardanoLib else jormungandrLib).environments.${environment};
  cfg = jormungandrLib.mkConfig (envCfg // {
    trustedPeers = envCfg.daedalusPeers or envCfg.trustedPeers;
  });
  jormungandrConfigForCluster = builtins.toFile "jormungandr-config-${environment}.yaml" (builtins.toJSON cfg);

  installDirectorySuffix = {
    qa = "QA";
    selfnode = "Selfnode";
    itn_selfnode = "Selfnode - ITN";
    nightly = "Nightly";
    itn_rewards_v1 = "- Rewards v1";
    staging = "Staging";
    testnet = "Testnet";
  };

  spacedName = if environment == "mainnet" then "Daedalus" else "Daedalus ${installDirectorySuffix.${environment}}";

  dataDir.linux = "\${XDG_DATA_HOME}/Daedalus/${environment}";
  dataDir.macos64 = "\${HOME}/Library/Application Support/${spacedName}";
  dataDir.windows = "\${APPDATA}\\${spacedName}";

  # TODO, use backend
  nodeBin.linux = "jormungandr";
  nodeBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr.exe";
  nodeBin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr";
  walletBin.linux = "cardano-wallet-jormungandr";
  walletBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-wallet-jormungandr.exe";
  walletBin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/cardano-wallet-jormungandr";

  daedalusBin.linux = "daedalus-frontend";
  daedalusBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\${spacedName}.exe";
  daedalusBin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/Frontend";
  cliBin.linux = if backend == "jormungandr" then "jcli" else "cardano-cli";
  cliBin.windows = if backend == "jormungandr" then "\${DAEDALUS_INSTALL_DIRECTORY}\\jcli.exe" else "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-cli.exe";
  cliBin.macos64 = if backend == "jormungandr" then "\${DAEDALUS_INSTALL_DIRECTORY}/jcli" else (if devShell then "cardano-cli" else "\${DAEDALUS_INSTALL_DIRECTORY}/cardano-cli");
  launcherLogsPrefix.linux = "${dataDir.${os}}/Logs/";
  launcherLogsPrefix.windows = "Logs\\pub";
  launcherLogsPrefix.macos64 = "${dataDir.${os}}/Logs/pub";
  logsPrefix.linux = "${dataDir.${os}}/Logs";
  logsPrefix.windows = "Logs";
  logsPrefix.macos64 = "${dataDir.${os}}/Logs";

  block0Bin.linux = envCfg.block0bin;
  block0Bin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\block-0.bin";
  block0Bin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/block-0.bin";

  genesisPath.linux = ../utils/jormungandr/selfnode/genesis.yaml;
  genesisPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\genesis.yaml";
  genesisPath.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/genesis.yaml";

  secretPath.linux = ../utils/jormungandr/selfnode/secret.yaml;
  secretPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\secret.yaml";
  secretPath.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/secret.yaml";

  configPath.linux = ../utils/jormungandr/selfnode/config.yaml;
  configPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\config.yaml";
  configPath.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/config.yaml";

  selfnodeBlock0 = runCommandNative "selfnode-block0.bin" { buildInputs = [ cardano-wallet-native.jormungandr-cli ]; } ''
    jcli genesis encode --input ${genesisPath.linux} --output $out
  '';

  cfgPathForOs = {
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr-config.yaml";
    macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/jormungandr-config.yaml";
    linux = jormungandrConfigForCluster;
  };
  finalJormungandrCfgPath = if devShell then jormungandrConfigForCluster else cfgPathForOs.${os};

  tier2-cfg-files = let
    genesisFile = if (environment == "selfnode") then ../utils/cardano/selfnode/genesis.json else envCfg.genesisFile;
  in runCommand "tier2-cfg-files" {
    nodeConfig = builtins.toJSON (envCfg.nodeConfig // { GenesisFile = finalGenesisLocation.${os}; });
    topologyFile = if environment == "selfnode" then envCfg.topology else cardanoLib.mkEdgeTopology {
      inherit (envCfg) edgePort;
      edgeNodes = [ envCfg.relaysNew ];
    };
    passAsFile = [ "nodeConfig" ];
  } ''
    mkdir $out
    cp ${genesisFile} $out/genesis.json
    cp $nodeConfigPath $out/config.yaml
    cp $topologyFile $out/topology.yaml
    ${lib.optionalString (environment == "selfnode") ''
      cp ${envCfg.delegationCertificate} $out/delegation.cert
      cp ${envCfg.signingKey} $out/signing.key
    ''}
  '';

  finalGenesisLocation.linux = envCfg.genesisFile;
  finalGenesisLocation.macos64 = envCfg.genesisFile;
  finalGenesisLocation.windows = envCfg.genesisFile;

  byronConfigDir = {
    linux = tier2-cfg-files;
    macos64 = if devShell then tier2-cfg-files else "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources";
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}";
  };

  walletArgs = [
    "launch" ] ++
  (if (envCfg ? block0bin) then [
    "--genesis-block" block0Bin.${os}
  ] else [
    "--genesis-block-hash" "${jormungandrLib.environments.${environment}.genesisHash}"
  ]) ++ [
    "--state-dir" dataDir.${os}
    "--sync-tolerance" "${jormungandrLib.environments.${environment}.syncTolerance}"
    "--random-port"
    "--"
    "--config" finalJormungandrCfgPath
  ];
  walletArgsSelfnode = [
    "launch"
    "--node-port" "8888"
    "--port" "8088"
    "--state-dir" dataDir.${os}
    "--genesis-block" (if ((os == "linux") || devShell) then selfnodeBlock0 else block0Bin.${os})
    "--sync-tolerance" "600s"
    "--"
    "--secret" (if devShell then secretPath.linux else secretPath.${os})
    "--config" (if devShell then configPath.linux else configPath.${os})
  ];

  launcherConfig = {
    daedalusBin = daedalusBin.${os};
    walletLogging = true;
    stateDir = dataDir.${os};
    launcherLogsPrefix = launcherLogsPrefix.${os};
    workingDir = dataDir.${os};
    frontendOnlyMode = true;
    nodeLogPath = null;
    logsPrefix = if backend == "cardano" then "${logsPrefix.${os}}ByronReboot" else logsPrefix.${os};
    nodeImplementation = backend;
    nodeLogConfig = null;
    nodeTimeoutSec = 60;
    tlsPath = null;
    x509ToolPath = null;
    cluster = environment;

    updateWindowsRunner = if os == "windows" then "Installer.bat" else "";
    updaterPath = "/foo";
    updaterArgs = [];
    updateArchive = "/bar";
  } // (lib.optionalAttrs (backend == "cardano") {
    networkName = environment;
    nodeConfig = {
      kind = "byron";
      configurationDir = "";
      network = {
        configFile = "${byronConfigDir.${os}}${dirSep}config.yaml";
        genesisFile = "${byronConfigDir.${os}}${dirSep}genesis.json";
        genesisHash = if (environment != "selfnode") then envCfg.genesisHash else "";
        topologyFile = "${byronConfigDir.${os}}${dirSep}topology.yaml";
      };
      socketFile = if os != "windows" then "${dataDir.${os}}${dirSep}cardano-node.socket" else "\\\\.\\pipe\\cardano-node-${environment}";
    } // (lib.optionalAttrs (environment == "selfnode") {
      delegationCertificate = "${byronConfigDir.${os}}${dirSep}delegation.cert";
      signingKey = "${byronConfigDir.${os}}${dirSep}signing.key";
    });
    syncTolerance = "300s";
  }) // (lib.optionalAttrs (environment == "selfnode") {
    cliBin = cliBin.${os};
  }) // (lib.optionalAttrs (backend == "jormungandr") {
    block0Path = if (envCfg ? block0bin) then block0Bin.${os} else "";
    block0Hash = jormungandrLib.environments.${environment}.genesisHash;
    configPath = finalJormungandrCfgPath;
    walletBin = walletBin.${os};
    walletArgs = if environment == "itn_selfnode" then walletArgsSelfnode else walletArgs;
    nodeBin = nodeBin.${os};
    nodeArgs = [];
    syncTolerance = if (environment == "itn_selfnode") then "600s" else jormungandrLib.environments.${environment}.syncTolerance;
  }) // (lib.optionalAttrs (environment == "itn_selfnode") {
    block0Path = if ((os == "linux") || devShell) then selfnodeBlock0 else block0Bin.${os};
    block0Hash = builtins.replaceStrings ["\n"] [""] (builtins.readFile (runCommandNative "selfnode-block0.hash" { buildInputs = [ cardano-wallet-native.jormungandr-cli ]; } ''
      jcli genesis hash --input ${selfnodeBlock0} > $out
    ''));
    secretPath = if devShell then secretPath.linux else secretPath.${os};
    configPath = if devShell then configPath.linux else configPath.${os};
  });

  hasBlock0 = (environment != "itn_selfnode") && envCfg ? block0bin;

  installerConfig = {
    installDirectory = if os == "linux" then "Daedalus/${environment}" else spacedName;
    inherit spacedName;
    macPackageName = "Daedalus${environment}";
    dataDir = dataDir.${os};
    hasBlock0 = false;
  } // (lib.optionalAttrs (backend == "jormungandr") {
    inherit hasBlock0;
  }) // (lib.optionalAttrs ((backend == "jormungandr") && hasBlock0) {
    block0 = envCfg.block0bin;
  }) // (lib.optionalAttrs (environment == "itn_selfnode") {
    genesisPath = genesisPath.linux;
    secretPath = secretPath.linux;
    configPath = configPath.linux;
    block0 = selfnodeBlock0;
    hasBlock0 = true;
  });
in {
  inherit launcherConfig installerConfig;
  jormungandr-config = jormungandrConfigForCluster;
  cfg-files = runCommand "cfg-files" {
    installerConfig = builtins.toJSON installerConfig;
    launcherConfig = builtins.toJSON launcherConfig;
    passAsFile = [ "installerConfig" "launcherConfig" ];
  } ''
    mkdir $out
    cd $out
    ${if (backend == "jormungandr") then ''
      ${lib.optionalString (installerConfig.hasBlock0) "cp ${installerConfig.block0} block-0.bin"}
      ${if (environment == "itn_selfnode") then ''
        cp ${../utils/jormungandr/selfnode/config.yaml} config.yaml
        cp ${../utils/jormungandr/selfnode/genesis.yaml} genesis.yaml
        cp ${../utils/jormungandr/selfnode/secret.yaml} secret.yaml
      '' else "cp ${jormungandrConfigForCluster} jormungandr-config.yaml"}
    ''
    else ""
    }
    cp $installerConfigPath installer-config.json
    cp $launcherConfigPath launcher-config.yaml
  '';
} // { inherit tier2-cfg-files; }
