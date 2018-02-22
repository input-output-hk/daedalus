import { ipcMain } from 'electron';
import { includes } from 'lodash';
import fs from 'fs';
import path from 'path';
import { pubLogsFolderPath } from '../config';

const CHANNEL_NAME = 'get-logs';
const ALLOWED_LOGS = [
  'Daedalus.log',
  'launcher.log',
  'node.pub.log',
  'node.pub.0.log',
  'node.pub.1.log',
  'node.pub.2.log',
  'node.pub.3.log',
  'node.pub.4.log',
  'node.pub.5.log',
  'node.pub.6.log',
  'node.pub.7.log',
  'node.pub.8.log',
  'node.pub.9.log',
  'node.pub.10.log',
  'node.pub.11.log',
  'node.pub.12.log',
  'node.pub.13.log',
  'node.pub.14.log',
  'node.pub.15.log',
  'node.pub.16.log',
  'node.pub.17.log',
  'node.pub.18.log',
  'node.pub.19.log',
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
