// @flow
import { createWriteStream, readFileSync } from 'fs';
import { exec, spawn } from 'child_process';
import { BrowserWindow } from 'electron';
import { CardanoNode } from './CardanoNode';
import { prepareArgs } from './config';
import {
  NODE_KILL_TIMEOUT,
  NODE_SHUTDOWN_TIMEOUT,
  NODE_STARTUP_MAX_RETRIES,
  NODE_STARTUP_TIMEOUT,
  NODE_UPDATE_TIMEOUT,
} from '../config';
import { logger } from '../utils/logging';
import type { LauncherConfig } from '../config';
import type {
  CardanoNodeState,
  CardanoStatus,
  TlsConfig,
} from '../../common/types/cardano-node.types';
import {
  cardanoAwaitUpdateChannel,
  cardanoFaultInjectionChannel,
  cardanoRestartChannel,
  cardanoStateChangeChannel,
  getCachedCardanoStatusChannel,
  cardanoTlsConfigChannel,
  setCachedCardanoStatusChannel,
} from '../ipc/cardano.ipc';
import { safeExitWithCode } from '../utils/safeExitWithCode';

const startCardanoNode = (
  node: CardanoNode,
  launcherConfig: LauncherConfig
) => {
  const {
    walletBin,
    tlsPath,
    logsPrefix,
    workingDir,
    nodeBin,
    nodeImplementation,
    nodeConfig,
    cluster,
    block0Path,
    block0Hash,
    secretPath,
    configPath,
    syncTolerance,
    cliBin,
  } = launcherConfig;
  const walletArgs = prepareArgs(launcherConfig);
  const logFilePath = `${logsPrefix}/pub/`;
  const config = {
    walletBin,
    nodeBin,
    nodeImplementation,
    nodeConfig,
    logFilePath,
    tlsPath,
    walletArgs,
    workingDir,
    cluster,
    block0Path,
    block0Hash,
    secretPath,
    configPath,
    syncTolerance,
    cliBin,
    startupTimeout: NODE_STARTUP_TIMEOUT,
    startupMaxRetries: NODE_STARTUP_MAX_RETRIES,
    shutdownTimeout: NODE_SHUTDOWN_TIMEOUT,
    killTimeout: NODE_KILL_TIMEOUT,
    updateTimeout: NODE_UPDATE_TIMEOUT,
  };
  return node.start(config);
};

const restartCardanoNode = async (node: CardanoNode) => {
  try {
    await node.restart();
  } catch (error) {
    logger.error('Could not restart CardanoNode', { error });
  }
};

export const setupCardanoNodeMode = (
  launcherConfig: LauncherConfig,
  mainWindow: BrowserWindow
) => {
  const cardanoNode = new CardanoNode(
    logger,
    {
      // Dependencies on node.js apis are passed as props to ease testing
      spawn,
      exec,
      readFileSync,
      createWriteStream,
      broadcastTlsConfig: (config: ?TlsConfig) => {
        if (!mainWindow.isDestroyed())
          cardanoTlsConfigChannel.send(config, mainWindow);
      },
      broadcastStateChange: (state: CardanoNodeState) => {
        if (!mainWindow.isDestroyed())
          cardanoStateChangeChannel.send(state, mainWindow);
      },
    },
    {
      // CardanoNode lifecycle hooks
      onStarting: () => {},
      onRunning: () => {},
      onStopping: () => {},
      onStopped: () => {},
      onUpdating: () => {},
      onUpdated: () => {},
      onCrashed: code => {
        const restartTimeout = cardanoNode.startupTries > 0 ? 30000 : 0;
        logger.info(
          `CardanoNode crashed with code ${code}. Restarting in ${restartTimeout}ms …`,
          { code, restartTimeout }
        );
        setTimeout(() => restartCardanoNode(cardanoNode), restartTimeout);
      },
      onError: () => {},
      onUnrecoverable: () => {},
    }
  );

  startCardanoNode(cardanoNode, launcherConfig);

  getCachedCardanoStatusChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for cardano status', {
      status: cardanoNode.status,
    });
    return Promise.resolve(cardanoNode.status);
  });

  setCachedCardanoStatusChannel.onReceive((status: ?CardanoStatus) => {
    logger.info(
      'ipcMain: Received request from renderer to cache cardano status',
      { status }
    );
    cardanoNode.saveStatus(status);
    return Promise.resolve();
  });

  cardanoStateChangeChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for node state', {
      state: cardanoNode.state,
    });
    return Promise.resolve(cardanoNode.state);
  });

  cardanoTlsConfigChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for tls config');
    return Promise.resolve(cardanoNode.tlsConfig);
  });

  cardanoAwaitUpdateChannel.onReceive(() => {
    logger.info('ipcMain: Received request from renderer to await update');
    setTimeout(async () => {
      await cardanoNode.expectNodeUpdate();
      logger.info(
        'CardanoNode applied an update. Exiting Daedalus with code 20.'
      );
      safeExitWithCode(20);
    });
    return Promise.resolve();
  });

  cardanoRestartChannel.onReceive(() => {
    logger.info('ipcMain: Received request from renderer to restart node');
    return cardanoNode.restart(true); // forced restart
  });

  cardanoFaultInjectionChannel.onReceive(fault => {
    logger.info(
      'ipcMain: Received request to inject a fault into cardano node',
      { fault }
    );
    return cardanoNode.setFault(fault);
  });

  return cardanoNode;
};
