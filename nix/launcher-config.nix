{ backend ? "jormungandr"
, environment ? "staging"
, os ? "linux"
, jormungandrLib
}:
let
  dataDir.linux = "\${XDG_DATA_HOME}/Daedalus/${environment}";

  # TODO, use backend
  nodeBin.linux = "jormungandr";
  nodeBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jormungandr.exe";
  walletBin.linux = "cardano-wallet-jormungandr";
  walletBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-wallet-jormungandr.exe";

  daedalusBin.linux = "daedalus-frontend";
  daedalusBin.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\Daedalus.exe";
  cliPath.linux = "jcli";
  cliPath.windows = "\${DAEDALUS_INSTALL_DIRECTORY}\\jcli.exe";
  launcherLogsPrefix.linux = "${dataDir.${os}}/Logs/";
  launcherLogsPrefix.windows = "Logs\\pub";
  logsPrefix.linux = "${dataDir.${os}}/Logs";
  logsPrefix.windows = "Logs";

  launcherConfig = {
    walletBin = walletBin.${os};
    walletArgs = if environment != "selfnode"
      then
        let cfg = jormungandrLib.mkConfig jormungandrLib.environments.${environment};
        jormungandrConfigForCluster = builtins.toFile "jormungandr-config.yaml" (builtins.toJSON cfg);
        in [
          "launch"
          "--genesis-block-hash"
          "${jormungandrLib.environments.${environment}.genesisHash}"
          "--"
          "--config" "${jormungandrConfigForCluster}"
        ]
      else [
        "launch"
        "--node-port"
        "8888"
        "--port"
        "8088"
        "--state-dir"
        dataDir.${os}
        "--genesis-block"
        "${dataDir.${os}}/block0.bin"
        "--"
        "--secret"
        "${dataDir.${os}}/secret.yaml"
      ];

    nodeBin = nodeBin.${os};
    nodeArgs = [];

    daedalusBin = daedalusBin.${os};
    walletLogging = true;
    statePath = dataDir.${os};
    launcherLogsPrefix = launcherLogsPrefix.${os};
    cliBin = cliPath.${os};
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
