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
    shutdownTimeout: 30000,
    killTimeout: 30000,
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
    broadcastTlsConfig: (config: ?TlsConfig) => {
      if (!mainWindow.isDestroyed()) cardanoTlsConfigChannel.send(config, mainWindow);
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
    onUpdated: () => {},
    onCrashed: (code) => {
      const restartTimeout = cardanoNode.startupTries > 0 ? 30000 : 0;
      Logger.info(`CardanoNode crashed with code ${code}. Restarting in ${restartTimeout}ms …`);
      setTimeout(() => restartCardanoNode(cardanoNode), restartTimeout);
    },
    onError: () => {},
    onUnrecoverable: () => {}
  });
  startCardanoNode(cardanoNode, launcherConfig);

  cardanoStateChangeChannel.onReceive(() => {
    Logger.info('ipcMain: Received request from renderer for node state.');
    return Promise.resolve(cardanoNode.state);
  });
  cardanoTlsConfigChannel.onReceive(() => {
    Logger.info('ipcMain: Received request from renderer for tls config.');
    return Promise.resolve(cardanoNode.tlsConfig);
  });
  cardanoAwaitUpdateChannel.onReceive(() => {
    Logger.info('ipcMain: Received request from renderer to await update.');
    setTimeout(async () => {
      await cardanoNode.expectNodeUpdate();
      Logger.info('CardanoNode applied an update. Exiting Daedalus with code 20.');
      safeExitWithCode(20);
    });
    return Promise.resolve();
  });
  cardanoRestartChannel.onReceive(() => {
    Logger.info('ipcMain: Received request from renderer to restart node.');
    return cardanoNode.restart(true); // forced restart
  });

  return cardanoNode;
};
