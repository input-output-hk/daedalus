// @flow
import path from 'path';
import { app, dialog } from 'electron';
import { readLauncherConfig } from './utils/config';
import environment from '../common/environment';

// Make sure Daedalus is started with required configuration
const { NODE_ENV, LAUNCHER_CONFIG } = process.env;
const isProd = NODE_ENV === 'production';
const isStartedByLauncher = !!LAUNCHER_CONFIG;
if (!isStartedByLauncher) {
  const isWindows = process.platform === 'win32';
  const dialogTitle = 'Daedalus improperly started!';
  let dialogMessage;
  if (isProd) {
    dialogMessage = isWindows ?
      'Please start Daedalus using the icon in the Windows start menu or using Daedalus icon on your desktop.' :
      'Daedalus was launched without needed configuration. Please start Daedalus using the shortcut provided by the installer.';
  } else {
    dialogMessage = 'Daedalus should be started using nix-shell. Find more details here: https://github.com/input-output-hk/daedalus/blob/develop/README.md';
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
  statePath: string,
  nodePath: string,
  nodeArgs: Array<string>,
  tlsPath: string,
  reportServer?: string,
  nodeDbPath: string,
  logsPrefix: string,
  nodeLogConfig: string,
  nodeTimeoutSec: number,
  configuration: {
    filePath: string,
    key: string,
    systemStart: string,
    seed: string,
  }
};

export const APP_NAME = 'Daedalus';
export const launcherConfig: LauncherConfig = readLauncherConfig(LAUNCHER_CONFIG);
export const appLogsFolderPath = launcherConfig.logsPrefix;
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
export const ALLOWED_LOGS = ['Daedalus.log'];
export const ALLOWED_NODE_LOGS = new RegExp(/(node.json-)(\d{14}$)/);
export const ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
export const MAX_NODE_LOGS_ALLOWED = 3;
export const MAX_LAUNCHER_LOGS_ALLOWED = 3;

// CardanoNode config
export const NODE_STARTUP_TIMEOUT = 5000;
export const NODE_STARTUP_MAX_RETRIES = 5;
export const NODE_SHUTDOWN_TIMEOUT = environment.isTest ? 5000 : 10000;
export const NODE_KILL_TIMEOUT = environment.isTest ? 5000 : 10000;
export const NODE_UPDATE_TIMEOUT = environment.isTest ? 10000 : 60000;
