// @flow
import { createWriteStream, readFileSync } from 'fs';
import { spawn } from 'child_process';
import { BrowserWindow, ipcMain } from 'electron';
import { Logger } from '../../common/logging';
import { prepareArgs } from './config';
import { CardanoNode } from './CardanoNode';
import {
  cardanoTlsConfigChannel,
  cardanoRestartChannel,
  cardanoAwaitUpdateChannel,
  cardanoStateChangeChannel, cardanoFaultInjectionChannel
} from '../ipc/cardano.ipc';
import { safeExitWithCode } from '../utils/safeExitWithCode';
import type { TlsConfig, CardanoNodeState } from '../../common/types/cardanoNode.types';
import type { LauncherConfig } from '../config';
import { CHECK_DISK_SPACE, NO_DISK_SPACE } from '../../common/ipc-api';

ipcMain.on(CHECK_DISK_SPACE.SUCCESS, (event, response) => {
  console.log('event', event);
  console.log('response', response);
});

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
    shutdownTimeout: 10000,
    killTimeout: 10000,
    updateTimeout: 60000,
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

const onCardanoError = (mainWindow, code) => {
  Logger.info(`CardanoNode error with code ${code}`);
  if (code === 'notEnoughDiskSpace') {
    mainWindow.webContents.send(NO_DISK_SPACE);
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
    onError: onCardanoError.bind(this, mainWindow),
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
  cardanoFaultInjectionChannel.onReceive((fault) => {
    Logger.info(`ipcMain: Received request to inject a fault into cardano node: ${String(fault)}`);
    return cardanoNode.setFault(fault);
  });

  return cardanoNode;
};
