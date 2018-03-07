import { ipcMain } from 'electron';
import fs from 'fs';
import { Logger, stringifyError } from '../../app/utils/logging';

const CHANNEL_NAME = 'delete-compressed-logs';

export const DELETE_COMPRESSED_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
};

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
