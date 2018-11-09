// @flow
import path from 'path';
import { readLauncherConfig } from './utils/config';

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
export const launcherConfig: LauncherConfig = readLauncherConfig(process.env.LAUNCHER_CONFIG);
export const appLogsFolderPath = launcherConfig.logsPrefix;
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
export const ALLOWED_LOGS = ['Daedalus.log'];
export const ALLOWED_NODE_LOGS = new RegExp(/(node.json-)(\d{14}$)/);
export const ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
export const MAX_NODE_LOGS_ALLOWED = 3;
export const MAX_LAUNCHER_LOGS_ALLOWED = 3;
