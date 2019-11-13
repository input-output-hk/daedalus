{ backend ? "jormungandr"
, environment ? "staging"
, os ? "linux"
, jormungandrLib ? (import ../. {}).jormungandrLib
, runCommand
, lib
, devShell ? false
}:
let
  dirSep = if os == "windows" then "\\" else "/";
  cfg = jormungandrLib.mkConfig jormungandrLib.environments.${environment};
  jormungandrConfigForCluster = builtins.toFile "jormungandr-config-${environment}.yaml" (builtins.toJSON cfg);

  installDirectorySuffix.qa = "QA";
  installDirectorySuffix.selfnode = "SelfNode";
  installDirectorySuffix.nightly = "Nightly";
  installDirectorySuffix.itn_balance_check = "Incentivized Testnet (Balance Check)";

  spacedName = "Daedalus ${installDirectorySuffix.${environment}}";

  dataDir.linux = "\${XDG_DATA_HOME}/Daedalus/${environment}";
  dataDir.macos64 = "\${HOME}/Library/Application Support/Daedalus ${environment}";
  dataDir.windows = "\${APPDATA}\\Daedalus ${environment}";

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

  cfgPathForOs = {
    windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr-config.yaml";
    macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr-config.yaml";
    linux = jormungandrConfigForCluster;
  };
  finalJormungandrCfgPath = if devShell then jormungandrConfigForCluster else cfgPathForOs.${os};

  walletArgs = [
    "launch"
    "--genesis-block-hash" "${jormungandrLib.environments.${environment}.genesisHash}"
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
    "--genesis-block" "${dataDir.${os}}${dirSep}block0.bin"
    "--sync-tolerance" "300s"
    "--"
    "--secret" "${dataDir.${os}}${dirSep}secret.yaml"
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
    cliBin = cliBin.${os};
    workingDir = dataDir.${os};
    frontendOnlyMode = true;
    nodeLogPath = null;
    logsPrefix = logsPrefix.${os};
    nodeImplementation = backend;
    nodeLogConfig = null;
    nodeTimeoutSec = 60;
    tlsPath = null;
    x509ToolPath = null;

    updateWindowsRunner = if os == "windows" then "Installer.bat" else "";
    updaterPath = "/foo";
    updaterArgs = [];
    updateArchive = "/bar";
  };
  installerConfig = {
    installDirectory = if os == "linux" then "Daedalus/${environment}" else "Daedalus ${environment}";
    inherit spacedName;
    macPackageName = "Daedalus${installDirectorySuffix.${environment}}";
    dataDir = dataDir.${os};
  };
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
    ${lib.optionalString (environment != "selfnode") "cp ${jormungandrConfigForCluster} jormungandr-config.yaml"}
    cp $installerConfigPath installer-config.json
    cp $launcherConfigPath launcher-config.yaml
  '';
}
