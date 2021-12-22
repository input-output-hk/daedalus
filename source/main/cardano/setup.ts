import { BrowserWindow } from 'electron';
import { createWriteStream, readFileSync } from 'fs';
import { exec, spawn } from 'child_process';
import { CardanoNode } from './CardanoNode';
import { exportWallets } from './utils';
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
import type {
  ExportWalletsRendererRequest,
  CardanoFaultInjectionRendererRequest,
} from '../../common/ipc/api';
import {
  cardanoAwaitUpdateChannel,
  cardanoFaultInjectionChannel,
  cardanoRestartChannel,
  cardanoStateChangeChannel,
  getCachedCardanoStatusChannel,
  cardanoTlsConfigChannel,
  setCachedCardanoStatusChannel,
  exportWalletsChannel,
} from '../ipc/cardano.ipc';
import { safeExitWithCode } from '../utils/safeExitWithCode';

const restartCardanoNode = async (node: CardanoNode) => {
  try {
    await node.restart();
  } catch (error) {
    logger.error('Could not restart CardanoNode', {
      error,
    });
  }
};

/**
 * Configures, starts and manages the CardanoNode responding to node
 * state changes, app events and IPC messages coming from the renderer.
 *
 * @param launcherConfig {LauncherConfig}
 * @param mainWindow
 */
export const setupCardanoNode = (
  launcherConfig: LauncherConfig,
  mainWindow: BrowserWindow
): CardanoNode => {
  const {
    logsPrefix,
    nodeImplementation,
    nodeConfig,
    tlsPath,
    stateDir,
    cluster,
    configPath,
    syncTolerance,
    cliBin,
    isStaging,
    metadataUrl,
  } = launcherConfig;
  const logFilePath = `${logsPrefix}/pub/`;
  const config = {
    logFilePath,
    nodeImplementation,
    nodeConfig,
    tlsPath,
    stateDir,
    cluster,
    configPath,
    syncTolerance,
    cliBin,
    isStaging,
    metadataUrl,
    startupTimeout: NODE_STARTUP_TIMEOUT,
    startupMaxRetries: NODE_STARTUP_MAX_RETRIES,
    shutdownTimeout: NODE_SHUTDOWN_TIMEOUT,
    killTimeout: NODE_KILL_TIMEOUT,
    updateTimeout: NODE_UPDATE_TIMEOUT,
  };
  const cardanoNode = new CardanoNode(
    logger,
    {
      // Dependencies on node.js apis are passed as props to ease testing
      spawn,
      exec,
      readFileSync,
      createWriteStream,
      broadcastTlsConfig: (tlsConfig: TlsConfig | null | undefined) => {
        if (!mainWindow.isDestroyed())
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
          cardanoTlsConfigChannel.send(tlsConfig, mainWindow);
      },
      broadcastStateChange: (state: CardanoNodeState) => {
        if (!mainWindow.isDestroyed())
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
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
      onCrashed: (code) => {
        const restartTimeout = cardanoNode.startupTries > 0 ? 30000 : 1000;
        logger.info(
          `CardanoNode crashed with code ${code}. Restarting in ${restartTimeout}ms...`,
          {
            code,
            restartTimeout,
          }
        );
        setTimeout(() => restartCardanoNode(cardanoNode), restartTimeout);
      },
      onError: () => {},
      onUnrecoverable: () => {},
    },
    config
  );
  getCachedCardanoStatusChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for cardano status', {
      status: cardanoNode.status,
    });
    return Promise.resolve(cardanoNode.status);
  });
  setCachedCardanoStatusChannel.onReceive(
    (status: CardanoStatus | null | undefined) => {
      logger.info(
        'ipcMain: Received request from renderer to cache cardano status',
        {
          status,
        }
      );
      cardanoNode.saveStatus(status);
      return Promise.resolve();
    }
  );
  cardanoStateChangeChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for node state', {
      state: cardanoNode.state,
    });
    return Promise.resolve(cardanoNode.state);
  });
  cardanoTlsConfigChannel.onRequest(() => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('ipcMain: Received request from renderer for tls config');
    return Promise.resolve(cardanoNode.tlsConfig);
  });
  cardanoAwaitUpdateChannel.onReceive(() => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('ipcMain: Received request from renderer to await update');
    setTimeout(async () => {
      await cardanoNode.expectNodeUpdate();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.info(
        'CardanoNode applied an update. Exiting Daedalus with code 20.'
      );
      safeExitWithCode(20);
    });
    return Promise.resolve();
  });
  cardanoRestartChannel.onReceive(() => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('ipcMain: Received request from renderer to restart node');
    return cardanoNode.restart(true); // forced restart
  });
  cardanoFaultInjectionChannel.onReceive(
    (fault: CardanoFaultInjectionRendererRequest) => {
      logger.info(
        'ipcMain: Received request to inject a fault into cardano node',
        {
          fault,
        }
      );
      return cardanoNode.setFault(fault);
    }
  );
  exportWalletsChannel.onRequest(
    ({ exportSourcePath, locale }: ExportWalletsRendererRequest) => {
      logger.info('ipcMain: Received request from renderer to export wallets', {
        exportSourcePath,
      });
      return Promise.resolve(
        exportWallets(exportSourcePath, launcherConfig, mainWindow, locale)
      );
    }
  );
  return cardanoNode;
};
