// @flow
import { ipcMain } from 'electron';
import fs from 'fs';
import { Logger, stringifyError } from '../../common/logging';
import { DELETE_COMPRESSED_LOGS } from '../../common/ipc-api';

export default () => {
  ipcMain.on(DELETE_COMPRESSED_LOGS.REQUEST, (event, file) => {
    const sender = event.sender;
    try {
      fs.unlinkSync(file);
      Logger.info('DELETE_COMPRESSED_LOGS.SUCCESS');
      return sender.send(DELETE_COMPRESSED_LOGS.SUCCESS);
    } catch (error) {
      Logger.error('DELETE_COMPRESSED_LOGS.ERROR: ' + stringifyError(error));
      return sender.send(DELETE_COMPRESSED_LOGS.ERROR, error);
    }
  });
};
