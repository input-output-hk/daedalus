// @flow
import { ipcMain } from 'electron';
import { createSupportWindow } from '../windows/support';
import { SUPPORT_WINDOW } from '../../common/ipc-api';

export default () => {
  ipcMain.on(SUPPORT_WINDOW.OPEN, (event, info) => {
    const window = createSupportWindow();
    window.webContents.on('did-finish-load', () => {
      window.webContents.send(SUPPORT_WINDOW.INFO, info);
    });
  });
};
