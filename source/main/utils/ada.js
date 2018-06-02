import { spawn } from 'child_process';
import { createWriteStream, readFileSync } from 'fs';
import log from 'electron-log';
import { ipcMain, app } from 'electron';
import { UPDATE_API } from '../../common/ipc-api';


const yamljs = require('yamljs');

// debug, remove later
const { dialog } = require('electron');

let port = 8090;

/*
  * todo:
  * when cardano quits unexpectedly, restart it up to X times, and update the UI
  * dont bother trying to connect to the api until `Started` message arrives
  * optional?:
  * call subprocess.disconnect() when the user tries to close daedalus,then wait for
  * the child to die, and show a "shutting down..." status, after a timeout, kill the child
  */
export const setupCardano = function setupCardano(mainWindow) {
  function resendApiInfo(window) {
    window.send(UPDATE_API.REQUEST, {
      ca: global.ca,
      port
    });
  }
  ipcMain.on(UPDATE_API.CLIENT_REQUEST, (event) => {
    resendApiInfo(event.sender);
  });
  if (!process.env.LAUNCHER_CONFIG) {
    log.info('IPC: launcher config not found, assuming cardano is ran externally');
    // TODO, tell daedalus to use port 8090
    return;
  }
  const inputYaml = readFileSync(process.env.LAUNCHER_CONFIG, 'utf8');
  if (process.env.XDG_DATA_HOME === undefined) {
    process.env.XDG_DATA_HOME = process.env.HOME + '/.local/share/';
  }
  const finalYaml = inputYaml.replace(/\${([^}]+)}/g,
    (a, b) => {
      const res = process.env[b];
      if (res === undefined) {
        console.log('warning var undefined:', b);
        return '';
      }
      return res;
    });
  const launcherConfig = yamljs.parse(finalYaml);
  if (!launcherConfig.frontendOnlyMode) {
    log.info('IPC: launcher config says node is started by the launcher');
    // TODO, tell daedalus to use port 8090
    return;
  }
  const logfile = createWriteStream(launcherConfig.logsPrefix + '/cardano-node.log', { flags: 'a' });
  logfile.on('open', () => {
    log.info('IPC:cardano logfile opened');
    let extraArgs = [];
    if (launcherConfig.reportServer) extraArgs = extraArgs.concat(['--report-server', launcherConfig.reportServer]);
    if (launcherConfig.nodeDbPath) extraArgs = extraArgs.concat(['--db-path', launcherConfig.nodeDbPath]);
    if (launcherConfig.configuration.filePath) extraArgs = extraArgs.concat(['--configuration-file', launcherConfig.configuration.filePath]);
    if (launcherConfig.configuration.key) extraArgs = extraArgs.concat(['--configuration-key', launcherConfig.configuration.key]);
    if (launcherConfig.configuration.systemStart) extraArgs = extraArgs.concat(['--system-start', launcherConfig.configuration.systemStart]);
    if (launcherConfig.configuration.seed) extraArgs = extraArgs.concat(['--configuration-seed', launcherConfig.configuration.seed]);
    if (launcherConfig.logsPrefix) extraArgs = extraArgs.concat(['--logs-prefix', launcherConfig.logsPrefix]);
    log.info(`IPC: running ${launcherConfig.nodePath} with args ${launcherConfig.nodeArgs} and ${extraArgs}`);
    const subprocess = spawn(launcherConfig.nodePath
      , launcherConfig.nodeArgs.concat(extraArgs)
      , {
        stdio: ['inherit', logfile, logfile, 'ipc']
      });

    subprocess.on('message', (msg) => {
      log.info('IPC:got reply', JSON.stringify(msg));
      if (msg.Started) {
        log.info('IPC: backend started, CA updated');
        Object.assign(global, {
          ca: readFileSync(launcherConfig.tlsPath + '/ca/ca.crt'),
        });
      } else if (msg.ReplyPort) {
        port = msg.ReplyPort;
        resendApiInfo(mainWindow);
      }
    });
    subprocess.on('close', (code, signal) => {
      log.info('IPC:all stdio to child has been closed', code, signal);
    });
    subprocess.on('disconnect', () => {
      log.info('IPC:all IPC handles closed');
    });
    subprocess.on('error', (err) => {
      log.info('IPC:error:', err);
    });
    subprocess.on('exit', (code, signal) => {
      // TODO: give a better UI when it fails and auto-retry a few times
      //dialog.showErrorBox('Backend Cardano node crashed! Exiting!', JSON.stringify({ code, signal }));
      log.info('IPC:child exited', code, signal);
      //app.quit();
    });

    subprocess.send({ QueryPort: [] });
    app.on('before-quit', () => {
      log.info('IPC:before-quit, stopping cardano');
      if (subprocess) {
        log.info('IPC:disconnecting IPC channel');
        subprocess.disconnect();
      }
    });
  });
};
