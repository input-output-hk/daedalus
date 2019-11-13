// @flow
import path from 'path';
import { app, dialog } from 'electron';
import { readLauncherConfig } from './utils/config';
import { environment } from './environment';

const { isTest, isProduction, isBlankScreenFixActive } = environment;

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

/**
 * The shape of the config params, usually provided to the cadano-node launcher
 */
export type LauncherConfig = {
  frontendOnlyMode: boolean,
  stateDir: string,
  walletBin: string,
  walletArgs: Array<string>,
  nodeBin: string,
  cliBin: string,
  nodeImplementation: 'jormungandr' | 'cardano-node',
  nodeArgs: Array<string>,
  tlsPath: string,
  nodeDbPath: string,
  workingDir: string,
  logsPrefix: string,
  nodeLogConfig: string,
  nodeTimeoutSec: number,
  configuration: {
    filePath: string,
    key: string,
    systemStart: string,
    seed: string,
  },
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
export const MIN_WINDOW_CONTENT_HEIGHT = 600;

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

export const APP_NAME = 'Daedalus';
export const launcherConfig: LauncherConfig = readLauncherConfig(
  LAUNCHER_CONFIG
);
export const appLogsFolderPath = launcherConfig.logsPrefix;
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
export const appFolderPath = launcherConfig.workingDir;
export const { nodeDbPath } = launcherConfig;
export const stateDirectoryPath = launcherConfig.stateDir;
export const stateDrive = isWindows ? stateDirectoryPath.slice(0, 2) : '/';
export const ALLOWED_LOGS = [
  'Daedalus.json',
  'System-info.json',
  'Daedalus-versions.json',
  'State-snapshot.json',
];
export const ALLOWED_NODE_LOGS = new RegExp(/(node.log-)(\d{13}$)/);
export const ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
export const MAX_NODE_LOGS_ALLOWED = 3;
export const MAX_LAUNCHER_LOGS_ALLOWED = 3;

// We need to invert 'frontendOnlyMode' value received from the launcherConfig
// as this variable has an opposite meaning from the launcher's perspective.
// Launcher treats the 'frontendOnlyMode' set to 'true' as the case where Daedalus
// takes the responsiblity for launching and managing the cardano-node.
export const frontendOnlyMode = !launcherConfig.frontendOnlyMode;

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
