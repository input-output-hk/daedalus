import path from 'path';
import { app, crashReporter, ipcMain } from 'electron';
import Log from 'electron-log';
import getRuntimeFolderPath from './getRuntimeFolderPath';
import { daedalusLogger } from './remoteLog';

export const setupLogging = () => {
  const APP_NAME = 'Daedalus';
  // Configure default logger levels for console and file outputs
  const runtimeFolderPath = getRuntimeFolderPath(process.platform, process.env, APP_NAME);
  const appLogFolderPath = path.join(runtimeFolderPath, 'Logs');
  const logFilePath = path.join(appLogFolderPath, APP_NAME + '.log');
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
  app.setPath('temp', appLogFolderPath);

  // TODO: Update when endpoint is ready (crash reports are only saved locally for now)
  crashReporter.start({
    companyName: 'IOHK',
    productName: APP_NAME,
    submitURL: '',
    uploadToServer: false
  });
};
