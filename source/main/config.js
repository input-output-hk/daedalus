import path from 'path';
import getRuntimeFolderPath from './utils/getRuntimeFolderPath';
import { readLauncherConfig } from './cardano/config';

const { LAUNCHER_CONFIG } = process.env;

export const APP_NAME = 'Daedalus';
export const runtimeFolderPath = getRuntimeFolderPath(process.platform, process.env, APP_NAME);
export const appLogsFolderPath = (
  readLauncherConfig(LAUNCHER_CONFIG).logsPrefix || path.join(runtimeFolderPath, 'Logs')
);
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');

export const ALLOWED_LOGS = [
  'Daedalus.log',
];
export const ALLOWED_NODE_LOGS = new RegExp(/(node.json-)(\d{14}$)/);
export const ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
export const MAX_NODE_LOGS_ALLOWED = 3;
export const MAX_LAUNCHER_LOGS_ALLOWED = 3;
