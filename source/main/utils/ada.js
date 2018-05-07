import { spawn } from 'child_process';
import { createWriteStream } from 'fs';
import log from 'electron-log';
import { app } from 'electron';
const yamljs = require('yamljs');

/*
  * todo:
  * when running in development mode, it should assume cardano is already running on port 8090
  * when cardano quits unexpectedly, restart it up to X times, and update the UI
  * dont bother trying to connect to the api until `Started` message arrives
  * use `ReplyPort` to control where the api connects
  * optional?:
  * call subprocess.disconnect() when the user tries to close daedalus, then wait for the child to die, and show a "shutting down..." status, after a timeout, kill the child
  */
export const setupCardano = () => {
  if (!process.env.LAUNCHER_CONFIG) {
    log.info("IPC: launcher config not found, assuming cardano is ran externally");
    // TODO, tell daedalus to use port 8090
    return;
  }
  const launcherConfig = yamljs.parseFile(process.env.LAUNCHER_CONFIG);
  if (!launcherConfig.frontendOnlyMode) {
    log.info("IPC: launcher config says node is started by the launcher");
    // TODO, tell daedalus to use port 8090
    return;
  }
  var logfile = createWriteStream("cardano-node.log", { flags: "a" });
  logfile.on("open", function () {
    log.info("IPC:cardano logfile opened");
    var extraArgs = [];
    if (launcherConfig.reportServer)              extraArgs = extraArgs.concat([ "--report-server", launcherConfig.reportServer ]);
    if (launcherConfig.nodeDbPath)                extraArgs = extraArgs.concat([ "--db-path", launcherConfig.nodeDbPath ]);
    if (launcherConfig.configuration.filePath)    extraArgs = extraArgs.concat([ "--configuration-file", launcherConfig.configuration.filePath ]);
    if (launcherConfig.configuration.key)         extraArgs = extraArgs.concat([ "--configuration-key", launcherConfig.configuration.key ]);
    if (launcherConfig.configuration.systemStart) extraArgs = extraArgs.concat([ "--system-start", launcherConfig.configuration.systemStart ]);
    if (launcherConfig.configuration.seed)        extraArgs = extraArgs.concat([ "--configuration-seed", launcherConfig.configuration.seed ]);
    if (launcherConfig.logsPrefix)                extraArgs = extraArgs.concat([ "--logs-prefix", launcherConfig.logsPrefix ]);
    log.info(`IPC: running ${launcherConfig.nodePath} with args ${launcherConfig.nodeArgs} and ${extraArgs}`);
    const subprocess = spawn(launcherConfig.nodePath
      , launcherConfig.nodeArgs.concat(extraArgs)
      , {
        stdio: [ "inherit", logfile, logfile, "ipc" ]
        , shell: true
      });

    subprocess.on("message", function (msg) {
      log.info("IPC:got reply",JSON.stringify(msg));
    });
    subprocess.on("close", function(code, signal) {
      log.info("IPC:all stdio to child has been closed", code, signal);
    });
    subprocess.on("disconnect", function() {
      log.info("IPC:all IPC handles closed");
    });
    subprocess.on("error", function (err) {
      log.info("IPC:error:", err);
    });
    subprocess.on("exit", function (code, signal) {
      log.info("IPC:child exited", code, signal);
    });

    subprocess.send({ QueryPort:[]});
    app.on('before-quit', () => {
      log.info("IPC:before-quit, stopping cardano");
      if (subprocess) {
        log.info("IPC:disconnecting IPC channel");
        subprocess.disconnect();
      }
    });
  });
};

