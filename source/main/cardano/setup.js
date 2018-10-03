// @flow
import { createWriteStream, readFileSync } from 'fs';
import { spawn } from 'child_process';
import { app, BrowserWindow } from 'electron';
import { Logger } from '../../common/logging';
import { prepareArgs, readLauncherConfig } from './config';
import { CardanoNode } from './CardanoNode';
import { CardanoNodeStates } from '../../common/types/cardanoNode.types';
import {
  cardanoTlsConfigChannel,
  cardanoRestartChannel,
  cardanoAwaitUpdateChannel,
  cardanoStateChangeChannel
} from '../ipc/cardano.ipc';
import { flushLogsAndExitWithCode } from '../utils/flushLogsAndExitWithCode';
import type { TlsConfig, CardanoNodeState } from '../../common/types/cardanoNode.types';
import type { LauncherConfig } from './config';

export const shouldCardanoBeLaunchedByDaedalus = (launcherConfig: Object): boolean => (
  launcherConfig.frontendOnlyMode
);

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

const restartOrExit = async (node: CardanoNode) => {
  try {
    await node.restart();
  } catch (error) {
    Logger.info(`Could not restart CardanoNode: ${error}`);
    Logger.info('Exiting Daedalus since CardanoNode is not starting.');
    flushLogsAndExitWithCode(0);
  }
};

export const loadLauncherConfig = (configPath?: ?string): ?LauncherConfig => {
  // Check if we should start cardano
  if (!configPath) return null;
  return readLauncherConfig(configPath);
};

/**
 * Configures, starts and manages the CardanoNode responding to node
 * state changes, app events and IPC messages coming from the renderer.
 *
 * @param launcherConfig {LauncherConfig}
 * @param mainWindow
 */
export const setupCardano = (launcherConfig: LauncherConfig, mainWindow: BrowserWindow) => {

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
      flushLogsAndExitWithCode(20);
    },
    onCrashed: (code) => {
      Logger.info(`CardanoNode exited unexpectatly with code ${code}. Restarting it …`);
      restartOrExit(cardanoNode);
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

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    event.preventDefault(); // prevent Daedalus from quitting immediately
    if (cardanoNode.state === CardanoNodeStates.STOPPING) return;
    try {
      Logger.info(`Daedalus:before-quit: stopping cardano-node with PID ${cardanoNode.pid || 'null'}`);
      await cardanoNode.stop();
      Logger.info('Daedalus:before-quit: exiting Daedalus with code 0.');
      flushLogsAndExitWithCode(0);
    } catch (stopError) {
      Logger.info(`Daedalus:before-quit: cardano-node did not exit correctly: ${stopError}`);
      flushLogsAndExitWithCode(0);
    }
  });

  return cardanoNode;
};
