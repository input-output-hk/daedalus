// @flow
import { createWriteStream } from 'fs';
import log from 'electron-log';
import { app, BrowserWindow, ipcMain } from 'electron';
import { ensureXDGDataIsSet, prepareArgs, readLauncherConfig } from './config';
import { CardanoNode } from './CardanoNode';
import { TLS_CONFIG_CHANNEL } from '../../common/ipc-api/tls-config';
import type { TlsConfig } from '../../common/ipc-api/tls-config';

export const shouldCardanoBeLaunchedByDaedalus = (launcherConfig: Object): boolean => {
  return launcherConfig.frontendOnlyMode;
};

/*
 * todo:
 * when cardano quits unexpectedly, restart it up to X times, and update the UI
 * dont bother trying to connect to the api until `Started` message arrives
 * optional?:
 * call subprocess.disconnect() when the user tries to close daedalus,then wait for
 * the child to die, and show a "shutting down..." status, after a timeout, kill the child
 */
export const setupCardano = (launcherConfigPath: string, mainWindow: BrowserWindow) => {
  // Check if we should start cardano
  if (!launcherConfigPath) {
    log.info('Launcher config not found, assuming cardano is ran externally');
    return;
  }
  const launcherConfig = readLauncherConfig(launcherConfigPath);
  if (!shouldCardanoBeLaunchedByDaedalus(launcherConfig)) {
    log.info('Launcher config says node is started by the launcher');
    return;
  }
  ensureXDGDataIsSet();

  // Start cardano-sl
  const { nodePath, tlsPath, logsPrefix } = launcherConfig;
  const nodeArgs = prepareArgs(launcherConfig);
  const config = {
    nodePath,
    tlsPath,
    nodeArgs,
    logFilePath: logsPrefix + '/cardano-node.log',
    startupTimeout: 5000,
    shutdownTimeout: 5000,
    killTimeout: 5000,
  };
  const cardanoNode = new CardanoNode(config, log, {
    sendTlsConfig: (tlsConfig: TlsConfig) => mainWindow.send(TLS_CONFIG_CHANNEL, tlsConfig),
    onExit: (code, wasExpected) => {
      if (wasExpected) {
        log.info('CardanoNode exited like expected. Exiting Daedalus with code 0.');
        app.exit(0);
      } else if (code === 20) {
        log.info('CardanoNode exited for applying an update. Exiting Daedalus with code 20.');
        app.exit(20);
      } else {
        log.info('CardanoNode exited unexpectatly. Restarting it …');
        cardanoNode.start();
      }
    }
  });
  cardanoNode.start();

  // Respond with TLS config whenever a render process asks for it
  ipcMain.on(TLS_CONFIG_CHANNEL, (event) => cardanoNode.sendTlsConfigTo(event.sender));

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    if (
      cardanoNode.state === CardanoNode.STOPPED ||
      cardanoNode.state === CardanoNode.STOPPING) {
      return;
    }
    event.preventDefault(); // prevent Daedalus from quitting immediately
    try {
      log.info(`Daedalus:before-quit, stopping cardano-node with PID ${cardanoNode.pid}`);
      await cardanoNode.stop();
    } catch (stopError) {
      log.info(`Daedalus:before-quit, cardano-node did not exit correctly: ${stopError}`);
      log.info('Exiting Daedalus with code 0.');
      app.exit(0);
    }
  });
};
