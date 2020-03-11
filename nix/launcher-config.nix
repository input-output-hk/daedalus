{ backend ? "jormungandr"
, environment ? "staging"
, os ? "linux"
, jormungandrLib ? (import ../. {}).jormungandrLib
, runCommand
, lib
, devShell ? false
, cardano-wallet-native
}:
let
  dirSep = if os == "windows" then "\\" else "/";
  envCfg = jormungandrLib.environments.${environment};
  cfg = jormungandrLib.mkConfig (envCfg // {
    trustedPeers = envCfg.daedalusPeers or envCfg.trustedPeers;
  });
  jormungandrConfigForCluster = builtins.toFile "jormungandr-config-${environment}.yaml" (builtins.toJSON cfg);

  installDirectorySuffix.qa = "QA";
  installDirectorySuffix.selfnode = "SelfNode";
  installDirectorySuffix.nightly = "Nightly";
  installDirectorySuffix.itn_rewards_v1 = "- Rewards v1";

  spacedName = "Daedalus ${installDirectorySuffix.${environment}}";

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
  cliBin.linux = "jcli";
  cliBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jcli.exe";
  cliBin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/jcli";
  launcherLogsPrefix.linux = "${dataDir.${os}}/Logs/";
  launcherLogsPrefix.windows = "Logs\\pub";
  launcherLogsPrefix.macos64 = "${dataDir.${os}}/Logs/pub";
  logsPrefix.linux = "${dataDir.${os}}/Logs";
  logsPrefix.windows = "Logs";
  logsPrefix.macos64 = "${dataDir.${os}}/Logs";

  block0Bin.linux = envCfg.block0bin;
  block0Bin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\block-0.bin";
  block0Bin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/block-0.bin";

  genesisPath.linux = ../utils/jormungandr/selfnode/genesis.yaml;
  genesisPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\genesis.yaml";
  genesisPath.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/genesis.yaml";

  secretPath.linux = ../utils/jormungandr/selfnode/secret.yaml;
  secretPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\secret.yaml";
  secretPath.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/secret.yaml";

  configPath.linux = ../utils/jormungandr/selfnode/config.yaml;
  configPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\config.yaml";
  configPath.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources/config.yaml";

  selfnodeBlock0 = runCommand "selfnode-block0.bin" { buildInputs = [ cardano-wallet-native.jormungandr-cli ]; } ''
    jcli genesis encode --input ${genesisPath.linux} --output $out
  '';

  cfgPathForOs = {
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr-config.yaml";
    macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr-config.yaml";
    linux = jormungandrConfigForCluster;
  };
  finalJormungandrCfgPath = if devShell then jormungandrConfigForCluster else cfgPathForOs.${os};

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
    walletBin = walletBin.${os};
    walletArgs = if environment == "selfnode" then walletArgsSelfnode else walletArgs;

    nodeBin = nodeBin.${os};
    nodeArgs = [];

    daedalusBin = daedalusBin.${os};
    walletLogging = true;
    stateDir = dataDir.${os};
    launcherLogsPrefix = launcherLogsPrefix.${os};
    workingDir = dataDir.${os};
    frontendOnlyMode = true;
    nodeLogPath = null;
    logsPrefix = logsPrefix.${os};
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
  } // (lib.optionalAttrs (environment == "selfnode") {
    block0Path = selfnodeBlock0;
    block0Hash = builtins.replaceStrings ["\n"] [""] (builtins.readFile (runCommand "selfnode-block0.hash" { buildInputs = [ cardano-wallet-native.jormungandr-cli ]; } ''
      jcli genesis hash --input ${selfnodeBlock0} > $out
    ''));
    secretPath = secretPath.linux;
    configPath = configPath.linux;
  });
  hasBlock0 = (environment != "selfnode") && envCfg ? block0bin;
  installerConfig = {
    installDirectory = if os == "linux" then "Daedalus/${environment}" else spacedName;
    inherit spacedName;
    macPackageName = "Daedalus${environment}";
    dataDir = dataDir.${os};
    inherit hasBlock0;
  } // (lib.optionalAttrs hasBlock0 {
    block0 = envCfg.block0bin;
  }) // (lib.optionalAttrs (environment == "selfnode") {
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
    ${if (environment == "selfnode") then ''
      cp ${../utils/jormungandr/selfnode/config.yaml} config.yaml
      cp ${../utils/jormungandr/selfnode/genesis.yaml} genesis.yaml
      cp ${../utils/jormungandr/selfnode/secret.yaml} secret.yaml
    '' else "cp ${jormungandrConfigForCluster} jormungandr-config.yaml"}
    cp $installerConfigPath installer-config.json
    cp $launcherConfigPath launcher-config.yaml
    ${lib.optionalString (installerConfig.hasBlock0) "cp ${installerConfig.block0} block-0.bin"}
  '';
}
