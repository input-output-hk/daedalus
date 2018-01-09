import { ipcMain } from 'electron';
import { map } from 'lodash';
import fs from 'fs';
import Log from 'electron-log';

const CHANNEL_NAME = 'delete-compressed-logs';

export const DELETE_COMPRESSED_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
};

export default () => {
  ipcMain.on(DELETE_COMPRESSED_LOGS.REQUEST, (event, files) => {
    const sender = event.sender;
    try {
      Log.info('Start deleting files');
      map(files, (file) => {
        fs.unlinkSync(file);
      });
      Log.warn('* files successfully deleted *');
      return sender.send(DELETE_COMPRESSED_LOGS.SUCCESS, 'Compressed files successfully removed');
    } catch (error) {
      Log.error('* file delete error! * ', error);
      return sender.send(DELETE_COMPRESSED_LOGS.ERROR, 'Erroor occured while try to delete compressed files');
    }
  });
};
