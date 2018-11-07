// @flow
import { ipcMain, BrowserWindow } from 'electron';
import { NO_DISK_SPACE, CHECK_DISK_SPACE } from '../../common/ipc-api';

export default () => {
  let checkedOnce = false;
  ipcMain.on(CHECK_DISK_SPACE.REQUEST, (event) => {
    let response;
    if (!checkedOnce) {
      checkedOnce = true;
      response = {
        diskSpaceRequired: 50,
        noDiskSpace: true
      };
    } else {
      checkedOnce = false;
      response = {
        diskSpaceRequired: 0,
        noDiskSpace: false
      };
    }
    setTimeout(() => {
      event.sender.send(CHECK_DISK_SPACE.SUCCESS, response);
    }, 2000);
  });
};

export const handleNoDiskSpace = (mainWindow: BrowserWindow) => {
  mainWindow.webContents.send(NO_DISK_SPACE);
};
