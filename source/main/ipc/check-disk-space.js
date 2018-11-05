// @flow
import { ipcMain } from 'electron';
import { CHECK_DISK_SPACE } from '../../common/ipc-api';

export default () => {
  ipcMain.on(CHECK_DISK_SPACE.REQUEST, (event) => {
    setTimeout(() => {
      event.sender.send(CHECK_DISK_SPACE.SUCCESS, false);
    }, 2000);
  });
};
