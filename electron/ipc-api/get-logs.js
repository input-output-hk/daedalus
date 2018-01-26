import { ipcMain } from 'electron';
import fs from 'fs';
import path from 'path';
import { APP_NAME, appLogsFolderPath } from '../config';

const CHANNEL_NAME = 'get-logs';

export const GET_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
};

export default () => {
  ipcMain.on(GET_LOGS.REQUEST, (event) => {
    const sender = event.sender;
    const pubLogsFolderPath = path.join(appLogsFolderPath, 'pub');

    // check if pub folder exists and create array of file names
    const pubLogFiles = [];
    if (fs.existsSync(pubLogsFolderPath)) {
      const files = fs.readdirSync(pubLogsFolderPath);
      for (let i = 0; i < files.length; i++) {
        const currentFile = path.join(pubLogsFolderPath, files[i]);
        if (fs.statSync(currentFile).isFile()) {
          pubLogFiles.push(currentFile.replace(pubLogsFolderPath + '/', ''));
        }
      }
    }

    // declare root logs, check which exists and create array of paths
    const rootLogFiles = [];
    const mainLogs = [`${APP_NAME}.log`, `${APP_NAME}.old.log`];
    for (let i = 0; i < mainLogs.length; i++) {
      if (fs.existsSync(path.join(appLogsFolderPath, mainLogs[i]))) {
        rootLogFiles.push(mainLogs[i]);
      }
    }

    const rootLogs = {
      path: appLogsFolderPath,
      files: rootLogFiles,
    };

    const pubLogs = {
      path: pubLogsFolderPath,
      files: pubLogFiles,
    };

    const logs = {
      rootLogs,
      pubLogs,
    };

    return sender.send(GET_LOGS.SUCCESS, logs);
  });
};
