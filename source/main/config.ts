import path from 'path';
import { app, dialog } from 'electron';
import { environment } from './environment';
import { readLauncherConfig } from './utils/config';
import { getBuildLabel } from '../common/utils/environmentCheckers';
import type { CardanoNodeImplementations } from '../common/types/cardano-node.types';

const {
  isTest,
  isProduction,
  isBlankScreenFixActive,
  current,
  build,
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
  configurationDir: string;
  delegationCertificate?: string;
  kind: 'byron' | 'shelley';
  network: {
    configFile: string;
    genesisFile: string;
    genesisHash: string;
    topologyFile: string;
  };
  signingKey?: string;
  rtsOpts?: Array<string>;
};

/**
 * The shape of the config params, usually provided to the cardano-node launcher
 */
export type LauncherConfig = {
  stateDir: string;
  nodeImplementation: CardanoNodeImplementations;
  nodeConfig: NodeConfig;
  tlsPath: string;
  logsPrefix: string;
  cluster: string;
  configPath: string;
  syncTolerance: string;
  cliBin: string;
  legacyStateDir: string;
  legacySecretKey: string;
  legacyWalletDB: string;
  isFlight: boolean;
  isStaging: boolean;
  smashUrl?: string;
  metadataUrl?: string;
  updateRunnerBin: string;
  selfnodeBin: string;
  mockTokenMetadataServerBin: string;
};
type WindowOptionsType = {
  show: boolean;
  width: number;
  height: number;
  webPreferences: {
    nodeIntegration: boolean;
    webviewTag: boolean;
    enableRemoteModule: boolean;
    preload: string;
  };
  icon?: string;
};
export const DEFAULT_WINDOW_WIDTH = 1150;
export const DEFAULT_WINDOW_HEIGHT = 870;
export const MIN_WINDOW_CONTENT_WIDTH = 905;
export const MIN_WINDOW_CONTENT_HEIGHT = 564;
export const windowOptions: WindowOptionsType = {
  show: false,
  width: DEFAULT_WINDOW_WIDTH,
  height: DEFAULT_WINDOW_HEIGHT,
  webPreferences: {
    nodeIntegration: isTest,
    webviewTag: false,
    enableRemoteModule: isTest,
    preload: path.join(__dirname, './preload.js'),
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ nodeIntegration: boolean; webviewTag: fals... Remove this comment to see the full error message
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
  legacyStateDir,
  logsPrefix,
  isFlight,
  smashUrl,
} = launcherConfig;
export const appLogsFolderPath = logsPrefix;
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
export const stateDirectoryPath = stateDir;
export const buildLabel = getBuildLabel(
  build,
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
  'cardano-wallet.log',
  'node.log',
];
export const ALLOWED_NODE_LOGS = new RegExp(/(node.log-)(\d{14}$)/);
export const ALLOWED_WALLET_LOGS = new RegExp(/(cardano-wallet.log-)(\d{14}$)/);
export const ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
export const MAX_NODE_LOGS_ALLOWED = 3;
export const MAX_WALLET_LOGS_ALLOWED = 3;
export const MAX_LAUNCHER_LOGS_ALLOWED = 3;
// CardanoNode config
export const NODE_STARTUP_TIMEOUT = 5000;
export const NODE_STARTUP_MAX_RETRIES = 5;
export const NODE_SHUTDOWN_TIMEOUT = isTest ? 5000 : 5 * 60 * 1000; // 5 minutes | unit: milliseconds

export const NODE_KILL_TIMEOUT = isTest ? 5000 : 5 * 60 * 1000; // 5 minutes | unit: milliseconds

export const NODE_UPDATE_TIMEOUT = isTest ? 10000 : 5 * 60 * 1000; // 5 minutes | unit: milliseconds

export const DISK_SPACE_REQUIRED = 2 * 1073741274; // 2 GB | unit: bytes

export const DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE = 10; // 10% of the available disk space

export const DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL = Number.MAX_SAFE_INTEGER; // Maximum interval

export const DISK_SPACE_CHECK_LONG_INTERVAL = 10 * 60 * 1000; // 10 minutes | unit: milliseconds

export const DISK_SPACE_CHECK_MEDIUM_INTERVAL = 60 * 1000; // 1 minute | unit: milliseconds

export const DISK_SPACE_CHECK_SHORT_INTERVAL = isTest ? 2000 : 10 * 1000; // 10 seconds | unit: milliseconds

export const DISK_SPACE_RECOMMENDED_PERCENTAGE = 15; // 15% of the total disk space

export const DISK_SPACE_CHECK_TIMEOUT = 9 * 1000; // Timeout for checking disks pace

export const BLOCK_REPLAY_PROGRESS_CHECK_INTERVAL = 1 * 1000; // 1 seconds | unit: milliseconds

// Used if token metadata server URL is not defined in launcher config
export const FALLBACK_TOKEN_METADATA_SERVER_URL =
  'https://metadata.cardano-testnet.iohkdev.io';
// Used by mock-token-metadata-server
export const MOCK_TOKEN_METADATA_SERVER_URL = 'http://localhost';
export const MOCK_TOKEN_METADATA_SERVER_PORT =
  process.env.MOCK_TOKEN_METADATA_SERVER_PORT || 0;
