// @flow
import fs from 'fs';
import path from 'path';
import log from 'electron-log-daedalus';
import ensureDirectoryExists from './ensureDirectoryExists';
import { pubLogsFolderPath, appLogsFolderPath, APP_NAME } from '../config';
import {
  constructMessageBody,
  formatMessage,
  stringifyData,
} from '../../common/utils/logging';
import { isFileNameWithTimestamp } from '../../common/utils/files';
import type {
  ConstructMessageBodyParams,
  MessageBody,
  LogSystemInfoParams,
  LogStateSnapshotParams,
} from '../../common/types/logging.types';

const isTest = process.env.NODE_ENV === 'test';
const isDev = process.env.NODE_ENV === 'development';

export const setupLogging = () => {
  const logFilePath = path.join(pubLogsFolderPath, `${APP_NAME}.json`);
  ensureDirectoryExists(pubLogsFolderPath);
  log.transports.console.level = isTest ? 'error' : 'info';
  log.transports.rendererConsole.level = isDev ? 'info' : 'error';
  log.transports.file.level = 'debug';
  log.transports.file.maxSize = 20 * 1024 * 1024;
  log.transports.file.file = logFilePath;
  log.transports.console.format = (message: Object): string =>
    formatMessage(message);

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
    const { message: msg, data = {} } = messageData;
    // log minimal message body in the renderer console
    let messageBody = { msg, data };
    if (typeof data === 'string') {
      messageBody = { ...messageBody, data: { response: data } };
    }
    return `[${year}T${time.slice(0, -1)}Z] ${context} ${stringifyData(
      messageBody
    )}`;
  };

  // Removes existing compressed logs
  fs.readdir(appLogsFolderPath, (err, files) => {
    files.filter(isFileNameWithTimestamp()).forEach(fileName => {
      const filePath = path.join(appLogsFolderPath, fileName);
      try {
        fs.unlinkSync(filePath);
      } catch (error) {
        // eslint-disable-next-line no-console
        console.error(
          `Compressed log file "${filePath}" deletion failed: ${error}`
        );
      }
    });
  });
};

export const logSystemInfo = (props: LogSystemInfoParams): MessageBody => {
  const { current, ...data } = props;
  const {
    network,
    osName,
    platformVersion,
    daedalusVersion,
    startTime: at,
  } = data;
  const env = `${network}:${osName}:${platformVersion}:${daedalusVersion}`;
  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env,
    ns: ['daedalus', `v${daedalusVersion}`, `*${current}*`],
    data,
    msg: 'Updating System-info.json file',
    pid: '',
    sev: 'info',
    thread: '',
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  const systemInfoFilePath = path.join(pubLogsFolderPath, 'System-info.json');
  fs.writeFileSync(systemInfoFilePath, JSON.stringify(messageBody));
  return messageBody;
};

export const logStateSnapshot = (props: LogStateSnapshotParams): MessageBody => {
  const { current, ...data } = props;
  const {
    daedalusVersion,
    cardanoVersion,
    cardanoNetwork,
    platform,
    startTime: at,
  } = data;
  const env = `${cardanoNetwork}:${platform}:${cardanoVersion}:${daedalusVersion}`;
  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env,
    ns: ['daedalus', `v${daedalusVersion}`, `*${current}*`],
    data,
    msg: 'Updating State-snapshot.json file',
    pid: '',
    sev: 'info',
    thread: '',
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  const stateSnapshotFilePath = path.join(pubLogsFolderPath, 'State-snapshot.json');
  fs.writeFileSync(stateSnapshotFilePath, JSON.stringify(messageBody));
  return messageBody;
};
