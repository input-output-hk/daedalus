'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.setupCardanoNode = void 0;
const fs_1 = require('fs');
const child_process_1 = require('child_process');
const CardanoNode_1 = require('./CardanoNode');
const utils_1 = require('./utils');
const config_1 = require('../config');
const logging_1 = require('../utils/logging');
const cardano_ipc_1 = require('../ipc/cardano.ipc');
const safeExitWithCode_1 = require('../utils/safeExitWithCode');
const restartCardanoNode = async (node) => {
  try {
    await node.restart();
  } catch (error) {
    logging_1.logger.error('Could not restart CardanoNode', {
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
 * @param rtsFlags flags used to start cardano-node
 */
const setupCardanoNode = (launcherConfig, mainWindow, rtsFlags) => {
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
    rtsFlags,
    isStaging,
    metadataUrl,
    startupTimeout: config_1.NODE_STARTUP_TIMEOUT,
    startupMaxRetries: config_1.NODE_STARTUP_MAX_RETRIES,
    shutdownTimeout: config_1.NODE_SHUTDOWN_TIMEOUT,
    killTimeout: config_1.NODE_KILL_TIMEOUT,
    updateTimeout: config_1.NODE_UPDATE_TIMEOUT,
  };
  const cardanoNode = new CardanoNode_1.CardanoNode(
    logging_1.logger,
    {
      // Dependencies on node.js apis are passed as props to ease testing
      spawn: child_process_1.spawn,
      exec: child_process_1.exec,
      readFileSync: fs_1.readFileSync,
      createWriteStream: fs_1.createWriteStream,
      broadcastTlsConfig: (tlsConfig) => {
        if (!mainWindow.isDestroyed())
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
          cardano_ipc_1.cardanoTlsConfigChannel.send(tlsConfig, mainWindow);
      },
      broadcastStateChange: (state) => {
        if (!mainWindow.isDestroyed())
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
          cardano_ipc_1.cardanoStateChangeChannel.send(state, mainWindow);
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
        logging_1.logger.info(
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
  cardano_ipc_1.getCachedCardanoStatusChannel.onRequest(() => {
    logging_1.logger.info(
      'ipcMain: Received request from renderer for cardano status',
      {
        status: cardanoNode.status,
      }
    );
    return Promise.resolve(cardanoNode.status);
  });
  cardano_ipc_1.setCachedCardanoStatusChannel.onReceive((status) => {
    logging_1.logger.info(
      'ipcMain: Received request from renderer to cache cardano status',
      {
        status,
      }
    );
    cardanoNode.saveStatus(status);
    return Promise.resolve();
  });
  cardano_ipc_1.cardanoStateChangeChannel.onRequest(() => {
    logging_1.logger.info(
      'ipcMain: Received request from renderer for node state',
      {
        state: cardanoNode.state,
      }
    );
    return Promise.resolve(cardanoNode.state);
  });
  cardano_ipc_1.cardanoTlsConfigChannel.onRequest(() => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      'ipcMain: Received request from renderer for tls config'
    );
    return Promise.resolve(cardanoNode.tlsConfig);
  });
  cardano_ipc_1.cardanoAwaitUpdateChannel.onReceive(() => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      'ipcMain: Received request from renderer to await update'
    );
    setTimeout(async () => {
      await cardanoNode.expectNodeUpdate();
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        'CardanoNode applied an update. Exiting Daedalus with code 20.'
      );
      (0, safeExitWithCode_1.safeExitWithCode)(20);
    });
    return Promise.resolve();
  });
  cardano_ipc_1.cardanoRestartChannel.onReceive(() => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      'ipcMain: Received request from renderer to restart node'
    );
    return cardanoNode.restart(true); // forced restart
  });
  cardano_ipc_1.cardanoFaultInjectionChannel.onReceive((fault) => {
    logging_1.logger.info(
      'ipcMain: Received request to inject a fault into cardano node',
      {
        fault,
      }
    );
    return cardanoNode.setFault(fault);
  });
  cardano_ipc_1.exportWalletsChannel.onRequest(
    ({ exportSourcePath, locale }) => {
      logging_1.logger.info(
        'ipcMain: Received request from renderer to export wallets',
        {
          exportSourcePath,
        }
      );
      return Promise.resolve(
        (0, utils_1.exportWallets)(
          exportSourcePath,
          launcherConfig,
          mainWindow,
          locale
        )
      );
    }
  );
  return cardanoNode;
};
exports.setupCardanoNode = setupCardanoNode;
//# sourceMappingURL=setup.js.map
