import { ipcMain } from 'electron';
import fs from 'fs';
import path from 'path';
import { pubLogsFolderPath } from '../config';

const CHANNEL_NAME = 'get-logs';

export const GET_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
};

export default () => {
  ipcMain.on(GET_LOGS.REQUEST, (event) => {
    const sender = event.sender;

    // check if pub folder exists and create array of log file names
    const logFiles = [];
    if (fs.existsSync(pubLogsFolderPath)) {
      const files = fs.readdirSync(pubLogsFolderPath);
      for (let i = 0; i < files.length; i++) {
        const currentFile = path.join(pubLogsFolderPath, files[i]);
        if (fs.statSync(currentFile).isFile()) {
          const isLogFile = path.extname(currentFile) === '.log';
          if (isLogFile) {
            logFiles.push(currentFile.replace(pubLogsFolderPath + '/', ''));
          }
        }
      }
    }

    const logs = {
      path: pubLogsFolderPath,
      files: logFiles,
    };

    return sender.send(GET_LOGS.SUCCESS, logs);
  });
};
