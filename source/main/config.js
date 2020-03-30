// @flow
import path from 'path';
import { app, dialog } from 'electron';
import { environment } from './environment';
import { readLauncherConfig } from './utils/config';
import { getBuildLabel } from '../common/utils/environmentCheckers';
import type { CardanoNodeImplementation } from '../common/types/cardano-node.types';

const {
  isTest,
  isProduction,
  isBlankScreenFixActive,
  current,
  buildNumber,
  network,
  version,
} = environment;

// Make sure Daedalus is started with required configuration
const { LAUNCHER_CONFIG } = process.env;
const isStartedByLauncher = !!LAUNCHER_CONFIG;
const isWindows = process.platform === 'win32';
if (!isStartedByLauncher) {
  const dialogTitle = 'Daedalus improperly started!';
  let dialogMessage;
  if (isProduction) {
    dialogMessage = isWindows
      ? 'Please start Daedalus using the icon in the Windows start menu or using Daedalus icon on your desktop.'
      : 'Daedalus was launched without needed configuration. Please start Daedalus using the shortcut provided by the installer.';
  } else {
    dialogMessage =
      'Daedalus should be started using nix-shell. Find more details here: https://github.com/input-output-hk/daedalus/blob/develop/README.md';
  }
  try {
    // app may not be available at this moment so we need to use try-catch
    dialog.showErrorBox(dialogTitle, dialogMessage);
    app.exit(1);
  } catch (e) {
    throw new Error(`${dialogTitle}\n\n${dialogMessage}\n`);
  }
}

export type NodeConfig = {
  configurationDir: string,
  delegationCertificate?: string,
  kind: 'byron',
  network: {
    configFile: string,
    genesisFile: string,
    genesisHash: string,
    topologyFile: string,
  },
  signingKey?: string,
};

/**
 * The shape of the config params, usually provided to the cadano-node launcher
 */
export type LauncherConfig = {
  stateDir: string,
  nodeImplementation: CardanoNodeImplementation,
  nodeConfig: NodeConfig,
  tlsPath: string,
  logsPrefix: string,
  cluster: string,
  block0Path: string,
  block0Hash: string,
  secretPath: string,
  configPath: string,
  syncTolerance: string,
  cliBin: string,
  exportWalletsBin: string,
  legacySecretKey: string,
  legacyWalletDB: string,
  isFlight: boolean,
};

type WindowOptionsType = {
  show: boolean,
  width: number,
  height: number,
  webPreferences: {
    nodeIntegration: boolean,
    webviewTag: boolean,
    enableRemoteModule: boolean,
    preload: string,
  },
  icon?: string,
};

export const WINDOW_WIDTH = 1150;
export const WINDOW_HEIGHT = 870;
export const MIN_WINDOW_CONTENT_WIDTH = 905;
export const MIN_WINDOW_CONTENT_HEIGHT = 700;

export const windowOptions: WindowOptionsType = {
  show: false,
  width: WINDOW_WIDTH,
  height: WINDOW_HEIGHT,
  webPreferences: {
    nodeIntegration: isTest,
    webviewTag: false,
    enableRemoteModule: isTest,
    preload: path.join(__dirname, './preload.js'),
    additionalArguments: isBlankScreenFixActive ? ['--safe-mode'] : [],
  },
  useContentSize: true,
};

export const launcherConfig: LauncherConfig = readLauncherConfig(
  LAUNCHER_CONFIG
);
export const {
  cluster,
  nodeImplementation,
  stateDir,
  logsPrefix,
  isFlight,
} = launcherConfig;
export const appLogsFolderPath = logsPrefix;
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
export const stateDirectoryPath = stateDir;
export const stateDrive = isWindows ? stateDirectoryPath.slice(0, 2) : '/';
export const buildLabel = getBuildLabel(
  buildNumber,
  network,
  current,
  isFlight,
  version
);

// Logging config
export const ALLOWED_LOGS = [
  'Daedalus.json',
  'System-info.json',
  'Daedalus-versions.json',
  'State-snapshot.json',
  'Wallet-migration-report.json',
  'node.log',
];
export const ALLOWED_NODE_LOGS = new RegExp(/(node.log-)(\d{14}$)/);
export const ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
export const MAX_NODE_LOGS_ALLOWED = 3;
export const MAX_LAUNCHER_LOGS_ALLOWED = 3;

// CardanoNode config
export const NODE_STARTUP_TIMEOUT = 5000;
export const NODE_STARTUP_MAX_RETRIES = 5;
export const NODE_SHUTDOWN_TIMEOUT = isTest ? 5000 : 10000;
export const NODE_KILL_TIMEOUT = isTest ? 5000 : 10000;
export const NODE_UPDATE_TIMEOUT = isTest ? 10000 : 60000;

export const DISK_SPACE_REQUIRED = 2 * 1073741274; // 2 GB | unit: bytes
export const DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE = 10; // 10% of the available disk space
export const DISK_SPACE_CHECK_LONG_INTERVAL = 10 * 60 * 1000; // 10 minutes | unit: milliseconds
export const DISK_SPACE_CHECK_MEDIUM_INTERVAL = 60 * 1000; // 1 minute | unit: milliseconds
export const DISK_SPACE_CHECK_SHORT_INTERVAL = isTest ? 2000 : 10 * 1000; // 10 seconds | unit: milliseconds
export const DISK_SPACE_RECOMMENDED_PERCENTAGE = 15; // 15% of the total disk space

// CardanoWallet config
export const STAKE_POOL_REGISTRY_URL = {
  itn_selfnode:
    'https://github.com/input-output-hk/daedalus/raw/selfnode/test-integration-registry.zip',
  nightly:
    'https://github.com/piotr-iohk/incentivized-testnet-stakepool-registry/archive/master.zip',
  qa:
    'https://explorer.qa.jormungandr-testnet.iohkdev.io/stakepool-registry/registry.zip',
};

// Cardano Byron Testnet network magic
export const TESTNET_MAGIC = 1097911063;
