// @flow
import { ipcMain } from 'electron';
import fs from 'fs';
import path from 'path';
import { appLogsFolderPath } from '../config';
import { Logger } from '../utils/logging';
import { WRITE_KEY_FILE } from '../../common/ipc-api';

export default () => {
  ipcMain.on(WRITE_KEY_FILE.REQUEST, (event, fileName, fileContent) => {
    const sender = event.sender;
    const outputPath = path.join(appLogsFolderPath, fileName);
    const output = fs.createWriteStream(outputPath);

    output.on('close', () => {
      Logger.debug('WRITE_KEY_FILE.SUCCESS', { outputPath });
      return sender.send(WRITE_KEY_FILE.SUCCESS, outputPath);
    });

    output.on('error', (error) => {
      Logger.error('WRITE_KEY_FILE.ERROR', { error });
      return sender.send(WRITE_KEY_FILE.ERROR, error);
    });

    Logger.debug('WRITE_KEY_FILE.START');

    output.write(fileContent);
    output.close();
  });
};
