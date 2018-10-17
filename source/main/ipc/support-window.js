import { ipcMain } from 'electron';
import fs from 'fs';
import path from 'path';
import { createSupportWindow } from '../windows/support';
import { SUPPORT_WINDOW } from '../../common/ipc-api';
import environment from '../../common/environment';

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

  const sendLogsInfo = (event, logsInfo) => {
    supportWindow.webContents.send(SUPPORT_WINDOW.LOGS_INFO, logsInfo);
  };

  ipcMain.on(SUPPORT_WINDOW.OPEN, (event, zendeskInfo) => {
    if (supportWindow) return;
    const { isMainnet, version, buildNumber } = environment;
    supportWindow = createSupportWindow(unsetSupportWindow);
    zendeskInfo = Object.assign(
      {},
      zendeskInfo,
      {
        network: isMainnet() ? 'mainnet' : 'testnet',
        version,
        buildNumber,
      }
    );
    supportWindow.webContents.on('did-finish-load', () => {
      supportWindow.webContents.send(SUPPORT_WINDOW.ZENDESK_INFO, zendeskInfo);
    });
  });

  ipcMain.on(SUPPORT_WINDOW.LOGS_INFO, (event, logsInfo) => {
    if (!supportWindow) return;
    fs.readFile(logsInfo.compressedLogsFile, (err, compressedLogsFileData) => {
      if (err) throw err;
      logsInfo = Object.assign(
        {},
        logsInfo,
        {
          compressedLogsFileData,
          compressedLogsFileName: path.basename(logsInfo.compressedLogsFile),
        }
      );
      supportWindow.webContents.on('did-finish-load', () => {
        sendLogsInfo(event, logsInfo);
      });
    });

  });

  ipcMain.on(SUPPORT_WINDOW.CLOSE, closeSupportWindow);
};
