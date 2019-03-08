// @flow
import { ipcMain } from 'electron';
import fs from 'fs';
import { Logger } from '../utils/logging';
import { DOWNLOAD_KEY_FILE } from '../../common/ipc-api';

export default () => {
  ipcMain.on(DOWNLOAD_KEY_FILE.REQUEST, (event, fileName, fileContent, filePath) => {
    const sender = event.sender;
    const output = fs.createWriteStream(filePath);

    output.on('close', () => {
      Logger.debug('DOWNLOAD_KEY_FILE.SUCCESS', { filePath });
      return sender.send(DOWNLOAD_KEY_FILE.SUCCESS, filePath);
    });

    output.on('error', (error) => {
      Logger.error('DOWNLOAD_KEY_FILE.ERROR', { error });
      return sender.send(DOWNLOAD_KEY_FILE.ERROR, error);
    });

    Logger.debug('DOWNLOAD_KEY_FILE.START');

    output.write(fileContent);
    output.close();
  });
};
