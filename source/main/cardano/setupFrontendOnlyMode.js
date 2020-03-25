// @flow
import { readFileSync } from 'fs';
import { BrowserWindow } from 'electron';
import {
  cardanoAwaitUpdateChannel,
  cardanoFaultInjectionChannel,
  cardanoRestartChannel,
  cardanoStateChangeChannel,
  getCachedCardanoStatusChannel,
  cardanoTlsConfigChannel,
  setCachedCardanoStatusChannel,
} from '../ipc/cardano.ipc';
import { logger } from '../utils/logging';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type { CardanoStatus } from '../../common/types/cardano-node.types';
import { safeExitWithCode } from '../utils/safeExitWithCode';

export const setupFrontendOnlyMode = (mainWindow: BrowserWindow) => {
  const { CARDANO_TLS_PATH, CARDANO_HOST, CARDANO_PORT } = process.env;

  if (!CARDANO_TLS_PATH) {
    throw new Error('CARDANO_TLS_PATH must be set in frontendOnlyMode');
  }

  const cardanoTlsPath = CARDANO_TLS_PATH;
  const cardanoHost = CARDANO_HOST || 'localhost';
  const cardanoPort = parseInt(CARDANO_PORT, 10) || 8090;
  const tlsConfig = {
    ca: readFileSync(`${cardanoTlsPath}/client/ca.crt`),
    key: readFileSync(`${cardanoTlsPath}/client/client.key`),
    cert: readFileSync(`${cardanoTlsPath}/client/client.pem`),
    hostname: cardanoHost,
    port: cardanoPort,
  };

  getCachedCardanoStatusChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for cardano status');
    return Promise.resolve(null);
  });

  setCachedCardanoStatusChannel.onReceive((status: ?CardanoStatus) => {
    logger.info(
      'ipcMain: Received request from renderer to cache cardano status',
      { status }
    );
    return Promise.resolve();
  });

  cardanoStateChangeChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for node state', {
      state: CardanoNodeStates.RUNNING,
    });
    return Promise.resolve(CardanoNodeStates.RUNNING);
  });

  cardanoTlsConfigChannel.onRequest(() => {
    logger.info('ipcMain: Received request from renderer for tls config');
    return Promise.resolve(tlsConfig);
  });

  cardanoAwaitUpdateChannel.onReceive(() => {
    logger.info('ipcMain: Received request from renderer to await update');
    safeExitWithCode(20);
    return Promise.resolve();
  });

  cardanoRestartChannel.onReceive(() => {
    logger.info('ipcMain: Received request from renderer to restart node');
    cardanoStateChangeChannel.send(CardanoNodeStates.STARTING, mainWindow);
    setTimeout(() => {
      if (!mainWindow.isDestroyed()) {
        cardanoStateChangeChannel.send(CardanoNodeStates.RUNNING, mainWindow);
      }
    }, 100);
    return Promise.resolve();
  });

  cardanoFaultInjectionChannel.onReceive(fault => {
    logger.info(
      'ipcMain: Received request to inject a fault into cardano node',
      { fault }
    );
    return Promise.reject(fault);
  });

  return null;
};
