// @flow
import * as fs from 'fs-extra';
import path from 'path';
import { BrowserWindow, dialog } from 'electron';
import { spawnSync } from 'child_process';
import { logger } from '../utils/logging';
import { TESTNET_MAGIC } from '../config';
import { getTranslation } from '../utils/getTranslation';
import ensureDirectoryExists from '../utils/ensureDirectoryExists';
import type { LauncherConfig } from '../config';
import type { ExportWalletsMainResponse } from '../../common/ipc/api';
import type {
  CardanoNodeStorageKeys,
  CardanoNodeImplementation,
  NetworkNames,
  PlatformNames,
  ProcessNames,
} from '../../common/types/cardano-node.types';
import {
  CardanoProcessNameOptions,
  NetworkNameOptions,
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
  nodeImplementation: CardanoNodeImplementation
): ProcessNames => ({
  CARDANO_PROCESS_NAME:
    CardanoProcessNameOptions[nodeImplementation][platform] ||
    (nodeImplementation === 'jormungandr' ? 'jormungandr' : 'cardano-node'),
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

  let legacySecretKeyPath = path.join(exportSourcePath, legacySecretKey);
  let legacyWalletDBPath = path.join(exportSourcePath, legacyWalletDB);

  // In case of Daedalus Flight build we need to copy over
  // legacySecretKey and legacyWalletDB from mainnet state dir
  // into Daedalus Flight state dir before extracting the wallets
  if (isFlight) {
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
  }

  // Export tool flags
  const exportWalletsBinFlags = [];

  // Cluster flags
  if (cluster === 'testnet') {
    exportWalletsBinFlags.push('--testnet', TESTNET_MAGIC);
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
  const wallets =
    [
      {
        encrypted_root_private_key:
          'b57361ebe335fa171a260fea7d3277579c212dc74fc2a408d6cbd8a6e7a847cab3c44c5fb190705ddd2698f2d5390798893349b4321e7474b1ce06c9d410b3d6055b42d4a95f19cb34b516a160a306c0eaef398e70ea91da450ccb2a7819e95b8c000436b43d5de6b0dd189cbfb0fc9ff954809abcb574d994cb5fafaf56b781',
        is_passphrase_empty: false,
        name: null,
        id: 'fec0c934fcaafb3ed7480132c654a8042312ab55',
        passphrase_hash:
          '31347c387c317c5743434c66724244502b496255516a4e6b314347363772647945306f436a727a61314b48765a4d4554644e6666773d3d7c5642767a2f79573559322b4a6647526f473275633955743556473430786d584d42356167356b747a5673383d',
      },
      {
        encrypted_root_private_key:
          'a00710ce083a17b6b1659d1540bc09fa0bdb5cb7a2f3c0c90c1996109b99a44f4f85a565f97b0e3207cf7d5cb4e28f53d8f84c7c7efa1cdb4f727e87e1e0952096f9cfd23fe63058756563d95ae7227ac10d37c4bd88733a06420c76619d738fd1ac606295e3bd70ea9f68c78d505ad50a9f1de588845c96218a6e89903b8c71',
        is_passphrase_empty: true,
        name: null,
        id: '656e7fd40cd5d892a571537c41fe8505fb5e4bb5',
        passphrase_hash:
          '31347c387c317c57434159434d2f456a4c4578724c314f51454a76494376476f316846374c47357a46494c3259346a54572b4e2f673d3d7c306c386e372b526934707a746a4d70646f4b4c59624347546970615433775847644e30505675327a7445773d',
      },
      {
        encrypted_root_private_key:
          '264ee1d412910811d725af34969dec27574796eefafac980139d677daad6ab4e6e70308f309cdd6423257c776b790da99d0d35f59ed7a9a705b69a741ab1788b918a486c39b5f5917956aeb146ba1e0aa60e43b60cc1539c33ce3118a9ee8f0b42436f89ce135d6cb8ff7f440c52502d0e0d8d586afdded2c8326e644696cedc',
        is_passphrase_empty: false,
        name: "Alan's Wallet With Password",
        id: 'a94d44b7a2ba5430d9faf8e8fbb891bd7c3c5089',
        passphrase_hash:
          '31347c387c317c57434332307259687651514350326e54576f7146447a4f5759666a674a6f734a6a775476796e6a6267732f6967773d3d7c6645303073457243615258634966416c6a6838683852365a4c4f506a6e4c726e4e676c736e4d734b2f616b3d',
      },
      {
        encrypted_root_private_key:
          '80708d6f0593c4a52bc81bed82155e9ca128315257377712ea1835f25bea514e634b9f65b5b6ff8d017f5d68de34f25ef7db7a8ec57446d22b3dc0fadaa1fbc7f93ba45ee18a5b00b8ec708422f91bf722f46acbea56d99702ddbaff6c60118aa03470e7e10f4372bbdc74c0816605ee6a01239064b0569c0010b294938f1e43',
        is_passphrase_empty: true,
        name: "Alan's Wallet",
        id: 'f3007ab2e6c2d4eb9669b89ddf0263b0504596ce',
        passphrase_hash:
          '31347c387c317c574343754249695a6976317430637a5659357a36566a4b524c42526a305733386d786f434d6c4b616748575767513d3d7c32306a64587a48507670755a647039335344713178754856456179452f627a797448743661366d70432f4d3d',
      },
      {
        encrypted_root_private_key:
          '02fc72724cec5f9df9a06ca0019cca7d2ea0c4974fb1f4e29ee26c81856ed7c02db79337952571405415f6609076858371f11edf19aec233b3bf7e2735b41acfb88dbec8c3d8420249139bdbc470736312ae4806e8aa529ea7dc77ba9832eb34e7d1a7c87bb262899b66b468bcc58c29d4dfb1e12d65cd77fe28f654b92bc3e0',
        is_passphrase_empty: false,
        name: 'Test Wallet 2',
        id: 'da4e278479937972e0e84f8418c97bf133c933db',
        passphrase_hash:
          '31347c387c317c5743444c4573316d7042777778616843626a6d3874396d45786a7a784e756d62792b3074654d30646138625977773d3d7c2b4647386344366549346a47665a436c4c3346706e747a78617537334c66515276716b386b7669325744553d',
      },
      {
        encrypted_root_private_key:
          '90a1b36331ed4a7f0f5243dc5cafc98218156a0750bdb401c87f6e828cb8fd5e0f6bd1342b8b22818e4de55518c121ba1350ea2f64421e7cd38f6a08dbabc47406283ca958aad4bb8ec95bf5d2770135c6335ee999cbd4fe96f03be3a214c9ef0d3a2930da0e3b335654ecf61f7172441e30ea22588db985673ef5c77783870a',
        is_passphrase_empty: true,
        name: 'Test Wallet 1',
        id: '8ea78091d689105f3ee1193f437d49fa2ca93945',
        passphrase_hash:
          '31347c387c317c5743427374436957746451306d5a70476b4f4454644f446765703964387177556978523242446f375470796435773d3d7c62447170494c2b4c305442356863664b5858572b716e49584459764a7132425463686930424a417a56474d3d',
      },
      {
        encrypted_root_private_key:
          '264ee1d412910811d725af34969dec27574796eefafac980139d677daad6ab4e6e70308f309cdd6423257c776b790da99d0d35f59ed7a9a705b69a741ab1788b918a486c39b5f5917956aeb146ba1e0aa60e43b60cc1539c33ce3118a9ee8f0b42436f89ce135d6cb8ff7f440c52502d0e0d8d586afdded2c8326e644696cedc',
        is_passphrase_empty: true,
        name: "Alan's Wallet With Password",
        id: 'a94d44b7a2ba5430d9faf8e8fbb891bd7c3c5089',
        passphrase_hash:
          '31347c387c317c57434332307259687651514350326e54576f7146447a4f5759666a674a6f734a6a775476796e6a6267732f6967773d3d7c6645303073457243615258634966416c6a6838683852365a4c4f506a6e4c726e4e676c736e4d734b2f616b3d',
      },
    ] || JSON.parse(stdout.toString() || '[]');
  const errors = stderr.toString();

  logger.info(`ipcMain: Exported ${wallets.length} wallets`, {
    walletsData: wallets.map(w => ({
      name: w.name,
      id: w.id,
      hasPassword: w.is_passphrase_empty,
    })),
    errors,
  });

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
        logger.info('ipcMain: Secret key file not found');
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
        logger.info('ipcMain: Wallet db directory not found');
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
