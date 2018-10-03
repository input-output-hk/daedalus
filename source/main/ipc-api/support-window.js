// @flow
import { ipcMain } from 'electron';
// import path from 'path';
import { createSupportWindow } from '../windows/support';
import { SUPPORT_WINDOW } from '../../common/ipc-api';
import { appLogsFolderPath, runtimeFolderPath, pubLogsFolderPath } from '../config';

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

  ipcMain.on(SUPPORT_WINDOW.OPEN, (event, zendeskInfo) => {
    if (supportWindow) return;
    supportWindow = createSupportWindow(unsetSupportWindow);
    supportWindow.webContents.on('did-finish-load', () => {
      supportWindow.webContents.send(SUPPORT_WINDOW.ZENDESK_INFO, zendeskInfo);
    });
  });

  ipcMain.on(SUPPORT_WINDOW.LOGS_INFO, (event, formInfo) => {
    if (!supportWindow) return;
    formInfo.appLogsFolderPath = appLogsFolderPath;
    formInfo.runtimeFolderPath = runtimeFolderPath;
    formInfo.pubLogsFolderPath = pubLogsFolderPath;
    supportWindow.webContents.send(SUPPORT_WINDOW.LOGS_INFO, formInfo);
    event.sender.send(SUPPORT_WINDOW.LOGS_INFO, formInfo);
  });

  ipcMain.on(SUPPORT_WINDOW.CLOSE, closeSupportWindow);
};
