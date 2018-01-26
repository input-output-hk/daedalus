import path from 'path';
import { app, crashReporter, ipcMain } from 'electron';
import Log from 'electron-log';
import { daedalusLogger } from './remoteLog';

import { appLogsFolderPath, APP_NAME } from '../config';

export const setupLogging = () => {
  // Configure default logger levels for console and file outputs
  const logFilePath = path.join(appLogsFolderPath, APP_NAME + '.log');
  Log.transports.console.level = 'warn';
  Log.transports.file.level = 'debug';
  Log.transports.file.file = logFilePath;

  try {
    let sendLogsToRemoteServer;
    ipcMain.on('send-logs-choice', (event, sendLogs) => {
      sendLogsToRemoteServer = sendLogs;
    });
    ipcMain.on('log-to-remote', (event, logEntry) => {
      if (sendLogsToRemoteServer) daedalusLogger.info(logEntry);
    });
  } catch (error) {
    Log.error('Error setting up log logging to remote server', error);
  }

  // Configure & start crash reporter
  app.setPath('temp', appLogsFolderPath);

  // TODO: Update when endpoint is ready (crash reports are only saved locally for now)
  crashReporter.start({
    companyName: 'IOHK',
    productName: APP_NAME,
    submitURL: '',
    uploadToServer: false
  });
};
