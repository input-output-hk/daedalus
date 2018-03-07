import { ipcMain } from 'electron';
import { includes } from 'lodash';
import fs from 'fs';
import path from 'path';
import { pubLogsFolderPath } from '../config';

const CHANNEL_NAME = 'get-logs';
const ALLOWED_LOGS = [
  'Daedalus.log',
  'launcher',
  'node.pub',
  'node.pub.0',
  'node.pub.1',
  'node.pub.2',
  'node.pub.3',
  'node.pub.4',
  'node.pub.5',
  'node.pub.6',
  'node.pub.7',
  'node.pub.8',
  'node.pub.9',
  'node.pub.10',
  'node.pub.11',
  'node.pub.12',
  'node.pub.13',
  'node.pub.14',
  'node.pub.15',
  'node.pub.16',
  'node.pub.17',
  'node.pub.18',
  'node.pub.19',
];

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
          const fileName = path.basename(currentFile);
          const isFileAllowed = includes(ALLOWED_LOGS, fileName);
          if (isFileAllowed) {
            logFiles.push(fileName);
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
