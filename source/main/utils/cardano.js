// @flow
import { createWriteStream } from 'fs';
import log from 'electron-log';
import { app, BrowserWindow, ipcMain } from 'electron';
import { ensureXDGDataIsSet, prepareArgs, readLauncherConfig } from '../cardano/config';
import { CardanoNode } from '../cardano/CardanoNode';
import { TLS_CONFIG_CHANNEL } from '../../common/ipc-api/tls-config';
import type { TlsConfig } from '../../common/ipc-api/tls-config';

/*
 * todo:
 * when cardano quits unexpectedly, restart it up to X times, and update the UI
 * dont bother trying to connect to the api until `Started` message arrives
 * optional?:
 * call subprocess.disconnect() when the user tries to close daedalus,then wait for
 * the child to die, and show a "shutting down..." status, after a timeout, kill the child
 */
export const setupCardano = (mainWindow: BrowserWindow) => {
  const { LAUNCHER_CONFIG } = process.env;
  if (!LAUNCHER_CONFIG) {
    log.info('IPC: launcher config not found, assuming cardano is ran externally');
    return;
  }
  ensureXDGDataIsSet();

  const launcherConfig = readLauncherConfig(LAUNCHER_CONFIG);
  if (!launcherConfig.frontendOnlyMode) {
    log.info('IPC: launcher config says node is started by the launcher');
    return;
  }
  const { nodePath, tlsPath, logsPrefix } = launcherConfig;

  const nodeArgs = prepareArgs(launcherConfig);
  const logFile = createWriteStream(logsPrefix + '/cardano-node.log', { flags: 'a' });
  const config = { nodePath, tlsPath, nodeArgs, logFile };
  const cardanoNode = new CardanoNode(config, log, {
    sendTlsConfig: (tlsConfig: TlsConfig) => mainWindow.send(TLS_CONFIG_CHANNEL, tlsConfig)
  });

  cardanoNode.start();

  ipcMain.on(TLS_CONFIG_CHANNEL, () => {
    cardanoNode.broadcastTlsConfig();
  });

  app.on('before-quit', () => {
    log.info(`Daedalus:before-quit, stopping cardano-node with PID ${cardanoNode.pid}`);
    cardanoNode.stop();
  });
};
