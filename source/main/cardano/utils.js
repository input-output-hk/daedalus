'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.exportWallets = exports.createSelfnodeConfig = exports.deriveProcessNames = exports.deriveStorageKeys = exports.promisedCondition = void 0;
const fs = __importStar(require('fs-extra'));
const path_1 = __importDefault(require('path'));
const electron_1 = require('electron');
const child_process_1 = require('child_process');
const logging_1 = require('../utils/logging');
const getTranslation_1 = require('../utils/getTranslation');
const ensureDirectoryExists_1 = __importDefault(
  require('../utils/ensureDirectoryExists')
);
const restoreKeystore_1 = require('../utils/restoreKeystore');
const cardano_node_types_1 = require('../../common/types/cardano-node.types');
const checkCondition = async (
  condition,
  resolve,
  reject,
  timeout,
  retryEvery,
  timeWaited = 0
) => {
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
const promisedCondition = (cond, timeout = 5000, retryEvery = 1000) =>
  new Promise((resolve, reject) => {
    checkCondition(cond, resolve, reject, timeout, retryEvery);
  });
exports.promisedCondition = promisedCondition;
const getNetworkName = (network) =>
  cardano_node_types_1.NetworkNameOptions[network] ||
  cardano_node_types_1.NetworkNameOptions.development;
const deriveStorageKeys = (network) => ({
  PREVIOUS_CARDANO_PID: `${getNetworkName(network)}-PREVIOUS-CARDANO-PID`,
});
exports.deriveStorageKeys = deriveStorageKeys;
const deriveProcessNames = (platform, nodeImplementation, isSelfnode) => ({
  CARDANO_PROCESS_NAME:
    cardano_node_types_1.CardanoProcessNameOptions[
      isSelfnode
        ? cardano_node_types_1.CardanoNodeImplementationOptions.SELFNODE
        : nodeImplementation
    ][platform] || 'cardano-node',
});
exports.deriveProcessNames = deriveProcessNames;
const createSelfnodeConfig = async (
  configFilePath,
  genesisFilePath,
  stateDir,
  cliBin
) => {
  const genesisFileExists = await fs.pathExists(genesisFilePath);
  if (!genesisFileExists) {
    throw new Error('No genesis file found');
  }
  const genesisFileContent = await fs.readJson(genesisFilePath);
  const startTime = Math.floor((Date.now() + 3000) / 1000);
  const genesisFile = JSON.stringify({ ...genesisFileContent, startTime });
  const genesisPath = path_1.default.join(stateDir, 'genesis.json');
  logging_1.logger.info('Creating selfnode genesis file...', {
    inputPath: genesisFilePath,
    outputPath: genesisPath,
    startTime,
  });
  await fs.remove(genesisPath);
  await fs.writeFile(genesisPath, genesisFile);
  logging_1.logger.info('Generating selfnode genesis hash...', {
    cliBin,
    genesisPath,
  });
  const { stdout: genesisHashBuffer } = (0, child_process_1.spawnSync)(cliBin, [
    'print-genesis-hash',
    '--genesis-json',
    genesisPath,
  ]);
  const genesisHash = genesisHashBuffer
    .toString()
    .replace('\r', '')
    .replace('\n', '');
  logging_1.logger.info('Generated selfnode genesis hash', {
    genesisHash,
  });
  const configFileExists = await fs.pathExists(configFilePath);
  if (!configFileExists) {
    throw new Error('No config file found');
  }
  const configFileContent = await fs.readFile(configFilePath);
  const configFile = JSON.stringify({
    ...JSON.parse(configFileContent.toString()),
    GenesisFile: genesisPath,
  });
  const configPath = path_1.default.join(stateDir, 'config.yaml');
  logging_1.logger.info('Creating selfnode config file...', {
    inputPath: configFilePath,
    outputPath: configPath,
    genesisPath,
    genesisHash,
  });
  await fs.remove(configPath);
  await fs.writeFile(configPath, configFile);
  const chainDir = path_1.default.join(stateDir, 'chain');
  logging_1.logger.info('Removing selfnode chain folder...', {
    chainDir,
  });
  await fs.remove(chainDir);
  const walletsDir = path_1.default.join(stateDir, 'wallets');
  logging_1.logger.info('Removing selfnode wallets folder...', {
    walletsDir,
  });
  await fs.remove(walletsDir);
  return {
    configPath,
    genesisPath,
    genesisHash,
  };
};
exports.createSelfnodeConfig = createSelfnodeConfig;
const exportWallets = async (
  exportSourcePath,
  launcherConfig,
  mainWindow,
  locale
) => {
  const {
    legacySecretKey,
    legacyWalletDB,
    stateDir,
    cluster,
    isFlight,
  } = launcherConfig;
  logging_1.logger.info('ipcMain: Starting wallets export...', {
    exportSourcePath,
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
    legacyWalletDBPath = path_1.default.join(
      exportSourcePath,
      '../..',
      legacyWalletDB
    );
  } else {
    legacySecretKeyPath = path_1.default.join(
      exportSourcePath,
      legacySecretKey
    );
    legacyWalletDBPath = path_1.default.join(exportSourcePath, legacyWalletDB);
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
      logging_1.logger.info('ipcMain: Exporting wallets failed', {
        errors: error,
      });
      return Promise.resolve({
        wallets: [],
        errors: error,
      });
    }
  }
  const legacyWalletDBPathExists = await fs.pathExists(
    `${legacyWalletDBPath}-acid`
  );
  logging_1.logger.info('ipcMain: Exporting wallets...', {
    legacySecretKeyPath,
    legacyWalletDBPath,
    legacyWalletDBPathExists,
  });
  let wallets = [];
  let errors = '';
  try {
    const legacySecretKeyFile = fs.readFileSync(legacySecretKeyPath);
    // @ts-ignore
    const rawWallets = await (0, restoreKeystore_1.decodeKeystore)(
      legacySecretKeyFile
    );
    wallets = rawWallets.map((w) => ({
      name: null,
      id: w.walletId,
      isEmptyPassphrase: w.isEmptyPassphrase,
      passphrase_hash: w.passphraseHash.toString('hex'),
      encrypted_root_private_key: w.encryptedPayload.toString('hex'),
    }));
  } catch (error) {
    errors = error.toString();
  }
  logging_1.logger.info(`ipcMain: Exported ${wallets.length} wallets`, {
    walletsData: wallets.map((w) => ({
      name: w.name,
      id: w.id,
      hasPassword: !w.isEmptyPassphrase,
    })),
    errors,
  });
  // Remove Daedalus migration data
  await removeMigrationData(stateDir);
  return Promise.resolve({
    wallets,
    errors,
  });
};
exports.exportWallets = exportWallets;
const prepareMigrationData = async (
  mainWindow,
  stateDir,
  legacySecretKey,
  legacyWalletDB,
  locale
) =>
  new Promise(async (resolve, reject) => {
    let legacySecretKeyPath = '';
    let legacyWalletDBPath = '';
    try {
      // Remove migration data dir if it exists
      const migrationDataDirPath = path_1.default.join(
        stateDir,
        'migration-data'
      );
      await fs.remove(migrationDataDirPath);
      (0, ensureDirectoryExists_1.default)(migrationDataDirPath);
      logging_1.logger.info(
        'ipcMain: Preparing Daedalus Flight migration data...',
        {
          migrationDataDirPath,
        }
      );
      const legacySecretKeyExists = await fs.pathExists(legacySecretKey);
      if (legacySecretKeyExists) {
        logging_1.logger.info('ipcMain: Copying secret key file...', {
          legacySecretKey,
        });
        legacySecretKeyPath = path_1.default.join(
          stateDir,
          'migration-data/secret.key'
        );
        await fs.copy(legacySecretKey, legacySecretKeyPath);
        logging_1.logger.info('ipcMain: Copied secret key file', {
          legacySecretKeyPath,
        });
      } else {
        logging_1.logger.info('ipcMain: Secret key file not found', {
          legacySecretKey,
        });
      }
      const legacyWalletDBFullPath = `${legacyWalletDB}-acid`;
      const legacyWalletDBPathExists = await fs.pathExists(
        legacyWalletDBFullPath
      );
      if (legacyWalletDBPathExists) {
        logging_1.logger.info('ipcMain: Copying wallet db directory...', {
          legacyWalletDBFullPath,
        });
        legacyWalletDBPath = path_1.default.join(
          stateDir,
          'migration-data/wallet-db-acid'
        );
        await fs.copy(legacyWalletDBFullPath, legacyWalletDBPath);
        legacyWalletDBPath = legacyWalletDBPath.replace('-acid', '');
        logging_1.logger.info('ipcMain: Copied wallet db directory', {
          legacyWalletDBPath,
        });
      } else {
        logging_1.logger.info('ipcMain: Wallet db directory not found', {
          legacyWalletDBFullPath,
        });
      }
      resolve({
        legacySecretKeyPath,
        legacyWalletDBPath,
      });
    } catch (error) {
      logging_1.logger.info(
        'ipcMain: Preparing Daedalus Flight migration data failed',
        {
          error,
        }
      );
      const { code } = error || {};
      if (code === 'EBUSY') {
        // "EBUSY" error happens on Windows when Daedalus mainnet is running during preparation
        // of Daedalus Flight wallet migration data as this prevents the files from being copied.
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          'ipcMain: Showing "Automatic wallet migration" warning...'
        );
        const { response } = await showExportWalletsWarning(mainWindow, locale);
        if (response === 0) {
          // User confirmed migration retry
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info(
            'ipcMain: User confirmed wallet migration retry'
          );
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
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.info('ipcMain: User canceled wallet migration');
          reject(error);
        }
      } else {
        reject(error);
      }
    }
  });
const removeMigrationData = async (stateDir) => {
  try {
    // Remove migration data dir if it exists
    const migrationDataDirPath = path_1.default.join(
      stateDir,
      'migration-data'
    );
    logging_1.logger.info(
      'ipcMain: Removing Daedalus Flight migration data...',
      {
        migrationDataDirPath,
      }
    );
    await fs.remove(migrationDataDirPath);
    logging_1.logger.info('ipcMain: Removed Daedalus Flight migration data', {
      migrationDataDirPath,
    });
  } catch (error) {
    logging_1.logger.info(
      'ipcMain: Removing Daedalus Flight migration data failed',
      {
        error,
      }
    );
  }
};
const showExportWalletsWarning = (mainWindow, locale) => {
  const translations = require(`../locales/${locale}`);
  const translation = (0, getTranslation_1.getTranslation)(
    translations,
    'dialog'
  );
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
  return electron_1.dialog.showMessageBox(
    mainWindow,
    exportWalletsDialogOptions
  );
};
//# sourceMappingURL=utils.js.map
