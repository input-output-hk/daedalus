// @flow
import { ipcMain } from 'electron';
import { createSupportWindow } from '../windows/support';
import { SUPPORT_WINDOW } from '../../common/ipc-api';

export default () => {
  let supportWindow;

  const closeSupportWindow = () => {
    if (supportWindow && supportWindow.webContents && supportWindow.webContents.send) {
      supportWindow.webContents.send(SUPPORT_WINDOW.CLOSE);
      supportWindow = null;
    }
  };

  const unsetSupportWindow = () => {
    supportWindow = null;
  };

  ipcMain.on(SUPPORT_WINDOW.OPEN, (event, info) => {
    if (supportWindow) return;
    supportWindow = createSupportWindow(unsetSupportWindow);
    supportWindow.webContents.on('did-finish-load', () => {
      supportWindow.webContents.send(SUPPORT_WINDOW.INFO, info);
    });
  });

  ipcMain.on(SUPPORT_WINDOW.CLOSE, closeSupportWindow);
};
