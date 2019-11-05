{ backend ? "jormungandr"
, environment ? "staging"
, os ? "linux"
, jormungandrLib
}:
let
  cfg = jormungandrLib.mkConfig jormungandrLib.environments.${environment};
  jormungandrConfigForCluster = builtins.toFile "jormungandr-config-${environment}.yaml" (builtins.toJSON cfg);

  dataDir.linux = "\${XDG_DATA_HOME}/Daedalus/${environment}";

  # TODO, use backend
  nodeBin.linux = "jormungandr";
  nodeBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr.exe";
  walletBin.linux = "cardano-wallet-jormungandr";
  walletBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-wallet-jormungandr.exe";

  daedalusBin.linux = "daedalus-frontend";
  daedalusBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\Daedalus.exe";
  cliBin.linux = "jcli";
  cliBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jcli.exe";
  launcherLogsPrefix.linux = "${dataDir.${os}}/Logs/";
  launcherLogsPrefix.windows = "Logs\\pub";
  logsPrefix.linux = "${dataDir.${os}}/Logs";
  logsPrefix.windows = "Logs";

  walletArgs.linux = [
    "launch"
    "--genesis-block-hash" "${jormungandrLib.environments.${environment}.genesisHash}"
    "--state-dir" dataDir.${os}
    "--"
    "--config" "${jormungandrConfigForCluster}"
  ];
  walletArgs.selfnode.linux = [
    "launch"
    "--node-port" "8888"
    "--port" "8088"
    "--state-dir" dataDir.${os}
    "--genesis-block" "${dataDir.${os}}/block0.bin"
    "--"
    "--secret" "${dataDir.${os}}/secret.yaml"
  ];

  launcherConfig = {
    walletBin = walletBin.${os};
    walletArgs = walletArgs.${environment}.${os} or walletArgs.${os};

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
in
  launcherConfig
