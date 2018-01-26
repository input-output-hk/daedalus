import path from 'path';
import getRuntimeFolderPath from './lib/getRuntimeFolderPath';

export const APP_NAME = 'Daedalus';
export const runtimeFolderPath = getRuntimeFolderPath(process.platform, process.env, APP_NAME);
export const appLogsFolderPath = path.join(runtimeFolderPath, 'Logs');
export const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');
