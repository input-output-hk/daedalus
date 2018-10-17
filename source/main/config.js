// @flow
import path from 'path';
import { readLauncherConfig } from './utils/config';

const { NODE_ENV, LAUNCHER_CONFIG } = process.env;
const isDev = NODE_ENV === 'development';
// Daedalus cannot proceed without a launcher config
if (isDev && !LAUNCHER_CONFIG) throw new Error('Daedalus should be started using nix-shell. Find more details here: https://github.com/input-output-hk/daedalus/blob/develop/README.md\n');

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
