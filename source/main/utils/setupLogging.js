import path from 'path';
import log from 'electron-log';
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
  log.transports.file.format = '[{y}-{m}-{d} {h}:{i}:{s}.{ms} {z}] [{level}] {text}';

};
