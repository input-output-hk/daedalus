// @flow
import { createWriteStream, readFileSync } from 'fs';
import { spawn } from 'child_process';
import { BrowserWindow } from 'electron';
import { Logger } from '../../common/logging';
import { prepareArgs } from './config';
import { CardanoNode } from './CardanoNode';
import {
  cardanoTlsConfigChannel,
  cardanoRestartChannel,
  cardanoAwaitUpdateChannel,
  cardanoStateChangeChannel
} from '../ipc/cardano.ipc';
import { safeExitWithCode } from '../utils/safeExitWithCode';
import type { TlsConfig, CardanoNodeState } from '../../common/types/cardanoNode.types';
import type { LauncherConfig } from '../config';

const startCardanoNode = (node: CardanoNode, launcherConfig: Object) => {
  const { nodePath, tlsPath, logsPrefix } = launcherConfig;
  const nodeArgs = prepareArgs(launcherConfig);
  const logFilePath = logsPrefix + '/cardano-node.log';
  const config = {
    nodePath,
    logFilePath,
    tlsPath,
    nodeArgs,
    startupTimeout: 5000,
    startupMaxRetries: 5,
    shutdownTimeout: 5000,
    killTimeout: 5000,
    updateTimeout: 20000,
  };
  return node.start(config);
};

const restartCardanoNode = async (node: CardanoNode) => {
  try {
    await node.restart();
  } catch (error) {
    Logger.info(`Could not restart CardanoNode: ${error}`);
  }
};

/**
 * Configures, starts and manages the CardanoNode responding to node
 * state changes, app events and IPC messages coming from the renderer.
 *
 * @param launcherConfig {LauncherConfig}
 * @param mainWindow
 */
export const setupCardano = (
  launcherConfig: LauncherConfig, mainWindow: BrowserWindow
): CardanoNode => {

  const cardanoNode = new CardanoNode(Logger, {
    // Dependencies on node.js apis are passed as props to ease testing
    spawn,
    readFileSync,
    createWriteStream,
    broadcastTlsConfig: (tlsConfig: TlsConfig) => {
      if (!mainWindow.isDestroyed()) cardanoTlsConfigChannel.send(tlsConfig, mainWindow);
    },
    broadcastStateChange: (state: CardanoNodeState) => {
      if (!mainWindow.isDestroyed()) cardanoStateChangeChannel.send(state, mainWindow);
    },
  }, {
    // CardanoNode lifecycle hooks
    onStarting: () => {},
    onRunning: () => {},
    onStopping: () => {},
    onStopped: () => {},
    onUpdating: () => {},
    onUpdated: () => {
      Logger.info('CardanoNode applied an update. Exiting Daedalus with code 20.');
      safeExitWithCode(20);
    },
    onCrashed: (code) => {
      Logger.info(`CardanoNode exited unexpectatly with code ${code}. Restarting it …`);
      restartCardanoNode(cardanoNode);
    },
    onError: () => {}
  });
  startCardanoNode(cardanoNode, launcherConfig);

  // Respond with TLS config whenever a render process asks for it
  cardanoTlsConfigChannel.onReceive(() => {
    Logger.info('ipcMain: Received request to send tls config to renderer.');
    return Promise.resolve(cardanoNode.tlsConfig);
  });
  // Handle update notification from frontend
  cardanoAwaitUpdateChannel.onReceive(() => {
    Logger.info('ipcMain: Received request from renderer to await update.');
    return cardanoNode.expectNodeUpdate();
  });
  // Restart cardano node if frontend requests it.
  cardanoRestartChannel.onReceive(() => {
    Logger.info('ipcMain: Received request from renderer to restart node.');
    return cardanoNode.restart();
  });

  return cardanoNode;
};
