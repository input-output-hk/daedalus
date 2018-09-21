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
