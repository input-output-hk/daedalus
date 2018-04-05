import path from 'path';
import log from 'electron-log';
import moment from 'moment';
import ensureDirectoryExists from './ensureDirectoryExists';
import { pubLogsFolderPath, APP_NAME } from '../config';

export const setupLogging = () => {
  const logFilePath = path.join(pubLogsFolderPath, APP_NAME + '.log');
  ensureDirectoryExists(pubLogsFolderPath);

  log.transports.console.level = false;
  log.transports.rendererConsole.level = 'warn';
  log.transports.file.level = 'debug';
  log.transports.file.maxSize = 20 * 1024 * 1024;
  log.transports.file.file = logFilePath;
  log.transports.file.format = (msg) => {
    const formattedDate = moment.utc(msg.date).format('YYYY-MM-DDTHH:mm:ss.0SSS');
    return `[${formattedDate}Z] [${msg.level}] ${msg.data}`;
  };
};
