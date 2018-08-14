import path from 'path';
import getRuntimeFolderPath from './utils/getRuntimeFolderPath';
import { launcherConfig } from './utils/launcherConfig';

export const APP_NAME = 'Daedalus';
export const runtimeFolderPath = getRuntimeFolderPath(process.platform, process.env, APP_NAME);
export const appLogsFolderPath = launcherConfig.logsPrefix || path.join(runtimeFolderPath, 'Logs');
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
