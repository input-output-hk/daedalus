// @flow
import fs from 'fs';
import path from 'path';
import log from 'electron-log';
import ensureDirectoryExists from './ensureDirectoryExists';
import { pubLogsFolderPath, appLogsFolderPath, APP_NAME } from '../config';
import { constructMessageBody, formatMessage, stringifyData } from '../../common/utils/logging';
import { isFileNameWithTimestamp } from '../../common/utils/files';
import type { ConstructMessageBodyParams, MessageBody } from '../../common/types/logging.types';

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
  log.transports.console.format = (message: Object): string => formatMessage(message);

  log.transports.file.format = (message: Object): string => {
    // Debug level logging is recorded as "info" in Daedalus log files
    // but in the same time we do not want to output it to console or terminal window
    const level = message.level === 'debug' ? 'info' : message.level;
    return formatMessage({ ...message, level });
  };

  log.transports.rendererConsole.format = (message: Object): string => {
    // deconstruct message data
    const date = message.date.toISOString();
    const [year, time] = date.split('T');
    const [context, messageData] = message.data;
    // construct a minimal message body for rendererConsole
    const { msg, data } = constructMessageBody({
      msg: messageData.message,
      data: messageData.data
    });
    return `${context} [${year} ${time.slice(0, -1)} UTC] ${stringifyData({ msg, data })}`;
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
  osName: string,
  platformVersion: string,
  ram: string,
  startTime: string,
};

export const updateUserSystemInfoLog = (props: Props): MessageBody => {
  const { current, ...data } = props;
  const { network, osName, platformVersion, daedalusVersion, startTime: at } = data;
  const env = `${network}:${osName}:${platformVersion}:${daedalusVersion}`;
  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env,
    ns: [
      'daedalus',
      `v${daedalusVersion}`,
      `*${current}*`,
    ],
    data,
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  fs.writeFileSync(
    path.join(pubLogsFolderPath, 'System-info.json'),
    JSON.stringify(messageBody)
  );
  return messageBody;
};
