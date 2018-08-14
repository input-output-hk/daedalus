// @flow
import { ipcMain } from 'electron';
import fs from 'fs';
import { DOWNLOAD_LOGS } from '../../common/ipc-api';

export default () => {
  ipcMain.on(DOWNLOAD_LOGS.REQUEST, (event, source, destination) => {
    const sender = event.sender;

    if (!fs.existsSync(source)) {
      return sender.send(DOWNLOAD_LOGS.ERROR, { message: 'File does not exist' });
    }

    const file = fs.readFileSync(source);
    fs.writeFileSync(destination, file);

    return sender.send(DOWNLOAD_LOGS.SUCCESS, { source, destination });
  });
};
