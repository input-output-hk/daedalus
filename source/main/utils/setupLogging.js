// @flow
import fs from 'fs';
import path from 'path';
import log from 'electron-log';
import moment from 'moment';
import ensureDirectoryExists from './ensureDirectoryExists';
import { pubLogsFolderPath, appLogsFolderPath, APP_NAME } from '../config';
import { isFileNameWithTimestamp } from '../../common/utils/files';

const isTest = process.env.NODE_ENV === 'test';
const isDev = process.env.NODE_ENV === 'development';

export const setupLogging = () => {
  const logFilePath = path.join(pubLogsFolderPath, APP_NAME + '.log');
  ensureDirectoryExists(pubLogsFolderPath);

  log.transports.console.level = isTest ? 'error' : 'info';
  log.transports.rendererConsole.level = isDev ? 'info' : 'error';
  log.transports.file.level = 'debug';
  log.transports.file.maxSize = 20 * 1024 * 1024;
  log.transports.file.file = logFilePath;
  log.transports.file.format = (msg) => {
    const formattedDate = moment.utc(msg.date).format('YYYY-MM-DDTHH:mm:ss.0SSS');
    // Debug level logging is recorded as "info" as we need it in Daedalus log files
    // but in the same time we do not want to output it to console or terminal window
    const level = msg.level === 'debug' ? 'info' : msg.level;
    return `[${formattedDate}Z] [${level}] ${msg.data}`;
  };

  // Removes existing compressed logs
  fs.readdir(appLogsFolderPath, (err, files) => {
    files
      .filter(isFileNameWithTimestamp())
      .forEach((logFileName) => {
        const logFile = path.join(appLogsFolderPath, logFileName);
        try {
          fs.unlinkSync(logFile);
        } catch (error) {
          console.error(`Compressed log file "${logFile}" deletion failed: ${error}`);
        }
      });
  });
};

type Props = {
  cardanoVersion: string,
  cpu: Array<Object>,
  current: string,
  daedalusVersion: string,
  isInSafeMode: string,
  network: string,
  platform: string,
  platformVersion: string,
  ram: string,
  startTime: string,
};

export const updateUserSystemInfoLog = (props: Props) => {
  const { current, ...data } = props;
  const { network, platform, platformVersion, daedalusVersion, startTime: at } = data;
  const env = `${network}:${platform}:${platformVersion}:${daedalusVersion}`;
  const output = {
    at,
    env,
    ns: [
      'daedalus',
      `*${current}*`,
    ],
    data,
    app: [
      'daedalus'
    ],
    msg: '',
    sev: '',
    thread: '',
  };

  fs.writeFileSync(
    path.join(pubLogsFolderPath, 'System-info.json'),
    JSON.stringify(output)
  );
};
