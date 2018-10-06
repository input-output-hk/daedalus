import { ipcMain } from 'electron';
import fs from 'fs';
import { createSupportWindow } from '../windows/support';
import { SUPPORT_WINDOW } from '../../common/ipc-api';

export default () => {
  let supportWindow;
  let didFinishLoad = false;

  const closeSupportWindow = () => {
    if (supportWindow && supportWindow.webContents && supportWindow.webContents.send) {
      supportWindow.webContents.send(SUPPORT_WINDOW.CLOSE);
      supportWindow = null;
    }
  };

  const unsetSupportWindow = () => {
    supportWindow = null;
  };

  const sendLogsInfo = (event, logsInfo) => {
    console.log('sendLogsInfo!!!!!!');
    supportWindow.webContents.send(SUPPORT_WINDOW.LOGS_INFO, logsInfo);
  };

  ipcMain.on(SUPPORT_WINDOW.OPEN, (event, zendeskInfo) => {
    if (supportWindow) return;
    supportWindow = createSupportWindow(unsetSupportWindow);
    supportWindow.webContents.on('did-finish-load', () => {
      didFinishLoad = true;
      supportWindow.webContents.send(SUPPORT_WINDOW.ZENDESK_INFO, zendeskInfo);
    });
  });

  ipcMain.on(SUPPORT_WINDOW.LOGS_INFO, (event, logsInfo) => {
    if (!supportWindow) return;
    fs.readFile(logsInfo.compressedLogsFile, (err, compressedLogsFileData) => {
      if (err) throw err;
      logsInfo.compressedLogsFileData = compressedLogsFileData;
      if (didFinishLoad) {
        sendLogsInfo(event, logsInfo);
      } else {
        supportWindow.webContents.on('did-finish-load', () => {
          sendLogsInfo(event, logsInfo);
        });
      }
    });

  });

  ipcMain.on(SUPPORT_WINDOW.CLOSE, closeSupportWindow);
};
