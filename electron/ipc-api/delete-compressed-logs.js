import { ipcMain } from 'electron';
import fs from 'fs';
import Log from 'electron-log';

const CHANNEL_NAME = 'delete-compressed-logs';

export const DELETE_COMPRESSED_LOGS = {
  REQUEST: CHANNEL_NAME,
  SUCCESS: `${CHANNEL_NAME}-success`,
};

export default () => {
  ipcMain.on(DELETE_COMPRESSED_LOGS.REQUEST, (event, path) => {
    const sender = event.sender;
    try {
      Log.info('Start deleting file: ', path);
      fs.unlinkSync(path);
      Log.info('* file successfully deleted *');
      return sender.send(DELETE_COMPRESSED_LOGS.SUCCESS, 'logs .zip file successfully removed');
    } catch (error) {
      Log.error('* file delete error! * ', error);
      return sender.send(DELETE_COMPRESSED_LOGS.ERROR, 'Erroor occured while try to delete logs .zip file');
    }
  });
};
