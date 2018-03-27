import path from 'path';
import { ipcMain } from 'electron';
import Log from 'electron-log';
import { daedalusLogger } from './remoteLog';
import ensureDirectoryExists from './ensureDirectoryExists';
import { pubLogsFolderPath, APP_NAME } from '../config';

export const setupLogging = () => {
  const logFilePath = path.join(pubLogsFolderPath, APP_NAME + '.log');
  ensureDirectoryExists(pubLogsFolderPath);

  Log.transports.console.level = 'warn';
  Log.transports.rendererConsole.level = 'warn';
  Log.transports.file.level = 'debug';
  Log.transports.file.maxSize = 20 * 1024 * 1024;
  Log.transports.file.file = logFilePath;
  Log.transports.file.format = '[{y}-{m}-{d} {h}:{i}:{s}.{ms} {z}] [{level}] {text}';

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
};
