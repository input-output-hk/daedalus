{ backend ? "jormungandr"
, environment ? "staging"
, os ? "linux"
, jormungandrLib ? (import ../. {}).jormungandrLib
}:
let
  cfg = jormungandrLib.mkConfig jormungandrLib.environments.${environment};
  jormungandrConfigForCluster = builtins.toFile "jormungandr-config-${environment}.yaml" (builtins.toJSON cfg);

  installDirectorySuffix.qa = "QA";
  installDirectorySuffix.selfnode = "SelfNode";
  installDirectorySuffix.nightly = "Nightly";

  dataDir.linux = "\${XDG_DATA_HOME}/Daedalus/${environment}";
  dataDir.macos64 = "\${HOME}/Library/Application Support/Daedalus/${environment}";
  dataDir.windows = "\${APPDATA}\\Daedalus\\${environment}";

  # TODO, use backend
  nodeBin.linux = "jormungandr";
  nodeBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr.exe";
  nodeBin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr";
  walletBin.linux = "cardano-wallet-jormungandr";
  walletBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-wallet-jormungandr.exe";
  walletBin.macos64 = "\${DAEDALUS_INSTALL_DIRECTORY}/cardano-wallet-jormungandr";

  daedalusBin.linux = "daedalus-frontend";
  daedalusBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\Daedalus.exe";
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

  walletArgs.linux = [
    "launch"
    "--genesis-block-hash" "${jormungandrLib.environments.${environment}.genesisHash}"
    "--state-dir" dataDir.${os}
    "--"
    "--config" "${jormungandrConfigForCluster}"
  ];
  walletArgs.macos64 = [
    "launch"
    "--genesis-block-hash" "${jormungandrLib.environments.${environment}.genesisHash}"
    "--state-dir" dataDir.${os}
    "--"
    "--config" "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr-config.yaml"
  ];
  walletArgs.windows = [
    "launch"
    "--genesis-block-hash" "${jormungandrLib.environments.${environment}.genesisHash}"
    "--state-dir" dataDir.${os}
    "--"
    "--config" "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr-config.yaml"
  ];
  walletArgs.selfnode.macos64 = [
    "launch"
    "--node-port" "8888"
    "--port" "8088"
    "--state-dir" dataDir.${os}
    "--genesis-block" "${dataDir.${os}}/block0.bin"
    "--"
    "--secret" "${dataDir.${os}}/secret.yaml"
  ];
  walletArgs.selfnode.windows = [
    "launch"
    "--node-port" "8888"
    "--port" "8088"
    "--state-dir" dataDir.${os}
    "--genesis-block" "${dataDir.${os}}/block0.bin"
    "--"
    "--secret" "${dataDir.${os}}/secret.yaml"
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
  installerConfig = {
    installDirectory = "Daedalus/${environment}";
    spacedName = "Daedalus ${installDirectorySuffix.${environment}}";
    macPackageName = "Daedalus${installDirectorySuffix.${environment}}";
    dataDir = dataDir.${os};
  };
in {
  inherit launcherConfig installerConfig;
  jormungandr-config = jormungandrConfigForCluster;
}
