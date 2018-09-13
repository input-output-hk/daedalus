// @flow
import { createWriteStream, readFileSync } from 'fs';
import { spawn } from 'child_process';
import log from 'electron-log';
import { app, BrowserWindow, ipcMain } from 'electron';
import { ensureXDGDataIsSet, prepareArgs, readLauncherConfig } from './config';
import { CardanoNode } from './CardanoNode';
import { TLS_CONFIG_CHANNEL } from '../../common/ipc-api/tls-config';
import { AWAIT_UPDATE_CHANNEL, CARDANO_NODE_STATE_CHANGE_CHANNEL } from '../../common/ipc-api';
import { CardanoNodeStates } from '../../common/types/cardanoNodeTypes';
import type { TlsConfig } from '../../common/ipc-api/tls-config';
import type { CardanoNodeState } from '../../common/types/cardanoNodeTypes';

export const shouldCardanoBeLaunchedByDaedalus = (launcherConfig: Object): boolean => (
  launcherConfig.frontendOnlyMode
);

const startCardanoNode = (node: CardanoNode, launcherConfig: Object) => {
  const { nodePath, tlsPath, logsPrefix } = launcherConfig;
  const nodeArgs = prepareArgs(launcherConfig);
  const logFilePath = logsPrefix + '/cardano-node.log';
  const config = {
    nodePath,
    tlsPath,
    nodeArgs,
    startupTimeout: 5000,
    shutdownTimeout: 5000,
    killTimeout: 5000,
    updateTimeout: 20000,
  };
  node.start(config, createWriteStream(logFilePath, { flags: 'a' }));
};

/**
 * Configures, starts and manages the CardanoNode responding to node
 * state changes, app events and IPC messages coming from the renderer.
 *
 * @param launcherConfigPath
 * @param mainWindow
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

  const cardanoNode = new CardanoNode(log, {
    spawn,
    readFileSync,
    broadcastTlsConfig: (tlsConfig: TlsConfig) => {
      if (!mainWindow.isDestroyed()) {
        mainWindow.send(TLS_CONFIG_CHANNEL, true, tlsConfig);
      }
    },
    broadcastStateChange: (state: CardanoNodeState) => {
      if (!mainWindow.isDestroyed()) {
        mainWindow.send(CARDANO_NODE_STATE_CHANGE_CHANNEL, true, state);
      }
    },
  }, {
    onStarting: () => {},
    onRunning: () => {},
    onStopping: () => {},
    onStopped: () => {
      log.info('CardanoNode exited like expected. Exiting Daedalus with code 0.');
      app.exit(0);
    },
    onUpdating: () => {},
    onUpdated: () => {
      log.info('CardanoNode applied an update. Exiting Daedalus with code 20.');
      app.exit(20);
    },
    onCrashed: (code) => {
      log.info(`CardanoNode exited unexpectatly with code ${code}. Restarting it …`);
      startCardanoNode(cardanoNode, launcherConfig);
    }
  });
  startCardanoNode(cardanoNode, launcherConfig);

  // Respond with TLS config whenever a render process asks for it
  ipcMain.on(TLS_CONFIG_CHANNEL, ({ sender }) => {
    log.info('ipcMain: Sending tls config to renderer.');
    sender.send(TLS_CONFIG_CHANNEL, true, cardanoNode.tlsConfig);
  });
  // Handle update notification from frontend
  ipcMain.on(AWAIT_UPDATE_CHANNEL, async ({ sender }) => {
    log.info('ipcMain: Received request from renderer to await update.');
    try {
      await cardanoNode.expectNodeUpdate();
      sender.send(AWAIT_UPDATE_CHANNEL, true);
    } catch (error) {
      sender.send(AWAIT_UPDATE_CHANNEL, false);
    }
  });

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    event.preventDefault(); // prevent Daedalus from quitting immediately
    if (cardanoNode.state === CardanoNodeStates.STOPPING) return;
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
