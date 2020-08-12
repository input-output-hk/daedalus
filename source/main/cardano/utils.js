// @flow
import * as fs from 'fs-extra';
import path from 'path';
import { BrowserWindow, dialog } from 'electron';
import { spawnSync } from 'child_process';
import { logger } from '../utils/logging';
import { getTranslation } from '../utils/getTranslation';
import ensureDirectoryExists from '../utils/ensureDirectoryExists';
import type { LauncherConfig } from '../config';
import type { ExportWalletsMainResponse } from '../../common/ipc/api';
import type {
  CardanoNodeStorageKeys,
  CardanoNodeImplementations,
  NetworkNames,
  PlatformNames,
  ProcessNames,
} from '../../common/types/cardano-node.types';
import {
  CardanoProcessNameOptions,
  CardanoNodeImplementationOptions,
  NetworkNameOptions,
  TESTNET_MAGIC,
} from '../../common/types/cardano-node.types';

export type Process = {
  pid: number,
  name: string,
  cmd: string,
  ppid?: number,
  cpu: number,
  memory: number,
};

const checkCondition = async (
  condition: () => boolean,
  resolve: Function,
  reject: Function,
  timeout: number,
  retryEvery: number,
  timeWaited: number = 0
): Promise<void> => {
  const result = await condition();
  if (result) {
    resolve();
  } else if (timeWaited >= timeout) {
    reject(`Promised condition not met within ${timeout}ms.`);
  } else {
    setTimeout(
      () =>
        checkCondition(
          condition,
          resolve,
          reject,
          timeout,
          retryEvery,
          timeWaited + retryEvery
        ),
      retryEvery
    );
  }
};

export const promisedCondition = (
  cond: Function,
  timeout: number = 5000,
  retryEvery: number = 1000
): Promise<void> =>
  new Promise((resolve, reject) => {
    checkCondition(cond, resolve, reject, timeout, retryEvery);
  });

const getNetworkName = (network: NetworkNames): string =>
  NetworkNameOptions[network] || NetworkNameOptions.development;

export const deriveStorageKeys = (
  network: NetworkNames
): CardanoNodeStorageKeys => ({
  PREVIOUS_CARDANO_PID: `${getNetworkName(network)}-PREVIOUS-CARDANO-PID`,
});

export const deriveProcessNames = (
  platform: PlatformNames,
  nodeImplementation: CardanoNodeImplementations
): ProcessNames => ({
  CARDANO_PROCESS_NAME:
    CardanoProcessNameOptions[nodeImplementation][platform] ||
    (nodeImplementation === CardanoNodeImplementationOptions.JORMUNGANDR
      ? 'jormungandr'
      : 'cardano-node'),
});

export const createSelfnodeConfig = async (
  configFilePath: string,
  genesisFilePath: string,
  stateDir: string,
  cliBin: string
): Promise<{
  configPath: string,
  genesisPath: string,
  genesisHash: string,
}> => {
  const genesisFileExists = await fs.pathExists(genesisFilePath);
  if (!genesisFileExists) {
    throw new Error('No genesis file found');
  }

  const genesisFileContent = await fs.readJson(genesisFilePath);
  const startTime = Math.floor((Date.now() + 3000) / 1000);
  const genesisFile = JSON.stringify({
    ...genesisFileContent,
    startTime,
  });
  const genesisPath = path.join(stateDir, 'genesis.json');

  logger.info('Creating selfnode genesis file...', {
    inputPath: genesisFilePath,
    outputPath: genesisPath,
    startTime,
  });

  await fs.remove(genesisPath);
  await fs.writeFile(genesisPath, genesisFile);

  logger.info('Generating selfnode genesis hash...', { cliBin, genesisPath });
  const { stdout: genesisHashBuffer } = spawnSync(cliBin, [
    'print-genesis-hash',
    '--genesis-json',
    genesisPath,
  ]);
  const genesisHash = genesisHashBuffer
    .toString()
    .replace('\r', '')
    .replace('\n', '');
  logger.info('Generated selfnode genesis hash', { genesisHash });

  const configFileExists = await fs.pathExists(configFilePath);
  if (!configFileExists) {
    throw new Error('No config file found');
  }

  const configFileContent = await fs.readFile(configFilePath);
  const configFile = JSON.stringify({
    ...JSON.parse(configFileContent),
    GenesisFile: genesisPath,
  });
  const configPath = path.join(stateDir, 'config.yaml');

  logger.info('Creating selfnode config file...', {
    inputPath: configFilePath,
    outputPath: configPath,
    genesisPath,
    genesisHash,
  });

  await fs.remove(configPath);
  await fs.writeFile(configPath, configFile);
  const chainDir = path.join(stateDir, 'chain');
  logger.info('Removing selfnode chain folder...', {
    chainDir,
  });
  await fs.remove(chainDir);

  const walletsDir = path.join(stateDir, 'wallets');
  logger.info('Removing selfnode wallets folder...', {
    walletsDir,
  });
  await fs.remove(walletsDir);

  return { configPath, genesisPath, genesisHash };
};

export const exportWallets = async (
  exportSourcePath: string,
  launcherConfig: LauncherConfig,
  mainWindow: BrowserWindow,
  locale: string
): Promise<ExportWalletsMainResponse> => {
  const {
    exportWalletsBin,
    legacySecretKey,
    legacyWalletDB,
    stateDir,
    cluster,
    isFlight,
  } = launcherConfig;

  logger.info('ipcMain: Starting wallets export...', {
    exportSourcePath,
    exportWalletsBin,
    legacySecretKey,
    legacyWalletDB,
    stateDir,
    cluster,
    isFlight,
  });

  let legacySecretKeyPath;
  let legacyWalletDBPath;

  if (exportSourcePath.endsWith('secret.key')) {
    legacySecretKeyPath = exportSourcePath;
    legacyWalletDBPath = path.join(exportSourcePath, '../..', legacyWalletDB);
  } else {
    legacySecretKeyPath = path.join(exportSourcePath, legacySecretKey);
    legacyWalletDBPath = path.join(exportSourcePath, legacyWalletDB);
  }

  // Prepare Daedalus migration data
  try {
    const response = await prepareMigrationData(
      mainWindow,
      stateDir,
      legacySecretKeyPath,
      legacyWalletDBPath,
      locale
    );
    legacySecretKeyPath = response.legacySecretKeyPath;
    legacyWalletDBPath = response.legacyWalletDBPath;
  } catch (error) {
    const { code } = error || {};
    if (code === 'EBUSY') {
      logger.info('ipcMain: Exporting wallets failed', {
        errors: error,
      });
      return Promise.resolve({ wallets: [], errors: error });
    }
  }

  // Export tool flags
  const exportWalletsBinFlags = [];

  // Cluster flags
  if (cluster === 'testnet') {
    exportWalletsBinFlags.push('--testnet', TESTNET_MAGIC.toString());
  } else {
    exportWalletsBinFlags.push('--mainnet');
  }

  // Secret key flags
  exportWalletsBinFlags.push('--keyfile', legacySecretKeyPath);

  // Wallet DB flags
  const legacyWalletDBPathExists = await fs.pathExists(
    `${legacyWalletDBPath}-acid`
  );
  if (legacyWalletDBPathExists) {
    exportWalletsBinFlags.push('--wallet-db-path', legacyWalletDBPath);
  }

  logger.info('ipcMain: Exporting wallets...', {
    exportWalletsBin,
    exportWalletsBinFlags,
  });

  const { stdout, stderr } = spawnSync(exportWalletsBin, exportWalletsBinFlags);
  const wallets = JSON.parse(stdout.toString() || '[]');
  const errors = stderr.toString();

  logger.info(`ipcMain: Exported ${wallets.length} wallets`, {
    walletsData: wallets.map(w => ({
      name: w.name,
      id: w.id,
      hasPassword: w.is_passphrase_empty,
    })),
    errors,
  });

  // Remove Daedalus migration data
  await removeMigrationData(stateDir);

  return Promise.resolve({ wallets, errors });
};

const prepareMigrationData = async (
  mainWindow: BrowserWindow,
  stateDir: string,
  legacySecretKey: string,
  legacyWalletDB: string,
  locale: string
): Promise<{
  legacySecretKeyPath: string,
  legacyWalletDBPath: string,
}> =>
  new Promise(async (resolve, reject) => {
    let legacySecretKeyPath = '';
    let legacyWalletDBPath = '';
    try {
      // Remove migration data dir if it exists
      const migrationDataDirPath = path.join(stateDir, 'migration-data');
      await fs.remove(migrationDataDirPath);
      ensureDirectoryExists(migrationDataDirPath);
      logger.info('ipcMain: Preparing Daedalus Flight migration data...', {
        migrationDataDirPath,
      });

      const legacySecretKeyExists = await fs.pathExists(legacySecretKey);
      if (legacySecretKeyExists) {
        logger.info('ipcMain: Copying secret key file...', {
          legacySecretKey,
        });
        legacySecretKeyPath = path.join(stateDir, 'migration-data/secret.key');
        await fs.copy(legacySecretKey, legacySecretKeyPath);
        logger.info('ipcMain: Copied secret key file', {
          legacySecretKeyPath,
        });
      } else {
        logger.info('ipcMain: Secret key file not found', {
          legacySecretKey,
        });
      }

      const legacyWalletDBFullPath = `${legacyWalletDB}-acid`;
      const legacyWalletDBPathExists = await fs.pathExists(
        legacyWalletDBFullPath
      );
      if (legacyWalletDBPathExists) {
        logger.info('ipcMain: Copying wallet db directory...', {
          legacyWalletDBFullPath,
        });
        legacyWalletDBPath = path.join(
          stateDir,
          'migration-data/wallet-db-acid'
        );
        await fs.copy(legacyWalletDBFullPath, legacyWalletDBPath);
        legacyWalletDBPath = legacyWalletDBPath.replace('-acid', '');
        logger.info('ipcMain: Copied wallet db directory', {
          legacyWalletDBPath,
        });
      } else {
        logger.info('ipcMain: Wallet db directory not found', {
          legacyWalletDBFullPath,
        });
      }
      resolve({ legacySecretKeyPath, legacyWalletDBPath });
    } catch (error) {
      logger.info('ipcMain: Preparing Daedalus Flight migration data failed', {
        error,
      });
      const { code } = error || {};
      if (code === 'EBUSY') {
        // "EBUSY" error happens on Windows when Daedalus mainnet is running during preparation
        // of Daedalus Flight wallet migration data as this prevents the files from being copied.
        logger.info('ipcMain: Showing "Automatic wallet migration" warning...');
        const { response } = await showExportWalletsWarning(mainWindow, locale);
        if (response === 0) {
          // User confirmed migration retry
          logger.info('ipcMain: User confirmed wallet migration retry');
          resolve(
            prepareMigrationData(
              mainWindow,
              stateDir,
              legacySecretKey,
              legacyWalletDB,
              locale
            )
          );
        } else {
          // User canceled migration
          logger.info('ipcMain: User canceled wallet migration');
          reject(error);
        }
      } else {
        reject(error);
      }
    }
  });

const removeMigrationData = async (stateDir: string) => {
  try {
    // Remove migration data dir if it exists
    const migrationDataDirPath = path.join(stateDir, 'migration-data');
    logger.info('ipcMain: Removing Daedalus Flight migration data...', {
      migrationDataDirPath,
    });
    await fs.remove(migrationDataDirPath);
    logger.info('ipcMain: Removed Daedalus Flight migration data', {
      migrationDataDirPath,
    });
  } catch (error) {
    logger.info('ipcMain: Removing Daedalus Flight migration data failed', {
      error,
    });
  }
};

const showExportWalletsWarning = (
  mainWindow: BrowserWindow,
  locale: string
): Promise<{ response: number }> => {
  const translations = require(`../locales/${locale}`);
  const translation = getTranslation(translations, 'dialog');
  const exportWalletsDialogOptions = {
    buttons: [
      translation('exportWalletsWarning.confirm'),
      translation('exportWalletsWarning.cancel'),
    ],
    type: 'warning',
    title: translation('exportWalletsWarning.title'),
    message: translation('exportWalletsWarning.message'),
    defaultId: 0,
    cancelId: 1,
    noLink: true,
  };
  return dialog.showMessageBox(mainWindow, exportWalletsDialogOptions);
};
