// @flow
import fs from 'fs';
import path from 'path';
import { pickBy } from 'lodash';
import type {
  FormatMessageContextParams,
  ConstructMessageBodyParams,
  MessageBody,
  ElectronLoggerMessage,
} from '../types/logging.types';
import { pubLogsFolderPath, APP_NAME } from '../../main/config';

const DEFAULT_MESSAGE_BODY = {
  ns: ['daedalus'],
  data: {},
  app: ['daedalus'],
};

const isProd = process.env.NODE_ENV === 'production';

const stringifyMessageBody = (messageBody: MessageBody): string => {
  const spacing = isProd ? 0 : 2;
  return JSON.stringify(messageBody, null, spacing);
};

export const filterLogData = (data: Object): Object => {
  const sensitiveData = [
    'spendingPassword',
    'oldPassword',
    'newPassword',
    'mnemonic',
    'recoveryPhrase',
    'passphrase',
    'password',
  ];
  return pickBy(data, (value, key) => {
    if (sensitiveData.includes(key)) {
      return false;
    }
    return true;
  });
};

export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) =>
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2);

export const formatContext = (context: FormatMessageContextParams): string => {
  const { appName, electronProcess, level, network } = context;
  return `[${appName}.*${network}*:${level}:${electronProcess}]`;
};

export const formatMessageTime = (date: Date): string => {
  const [year, time] = date.toISOString().split('T');
  return `[${year}T${time.slice(0, -1)}Z]`;
};

export const constructMessageBody = (
  bodyData: ConstructMessageBodyParams
): MessageBody => {
  let messageBody = { ...DEFAULT_MESSAGE_BODY, ...bodyData };
  if (typeof messageBody.data === 'string') {
    messageBody = { ...messageBody, data: { response: messageBody.data } };
  }
  const { at, env, ns, data, app, msg, pid, sev, thread } = messageBody;
  return { at, env, ns, data, app, msg, pid, sev, thread };
};

export const formatMessage = (loggerMessage: ElectronLoggerMessage): string => {
  const at = loggerMessage.date.toISOString();
  const [context, messageData] = loggerMessage.data;
  const { level } = loggerMessage;
  const { message: msg, data = {}, environmentData } = messageData;
  const { network, os, platformVersion, version } = environmentData;

  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env: `${network}:${os}:${platformVersion}`,
    ns: ['daedalus', `v${version}`, `*${network}*`],
    data,
    msg,
    pid: '',
    sev: level,
    thread: '',
  };

  const messageBody: MessageBody = constructMessageBody(messageBodyParams);

  if (isProd) return stringifyMessageBody(messageBody);

  const messageTime: string = formatMessageTime(loggerMessage.date);
  return `${messageTime} ${context} ${stringifyMessageBody(messageBody)}`;
};

const composeRotatedOldLogFiles = (compareLogFilePath: string) => {
  let index = 0;
  const rotatedLogFilePathes = [
    `${APP_NAME}.old.1.json`,
    `${APP_NAME}.old.2.json`,
  ];

  for (index = 0; index < 2; index++) {
    if (!fs.existsSync(rotatedLogFilePathes[index])) {
      break;
    }
  }
  if (index < 2) {
    fs.copyFileSync(compareLogFilePath, rotatedLogFilePathes[index]);
    fs.unlinkSync(compareLogFilePath);
    return;
  }

  fs.unlinkSync(rotatedLogFilePathes[0]);
  fs.copyFileSync(rotatedLogFilePathes[1], rotatedLogFilePathes[0]);
  fs.unlinkSync(rotatedLogFilePathes[1]);
  fs.copyFileSync(compareLogFilePath, rotatedLogFilePathes[1]);
  fs.unlinkSync(compareLogFilePath);
};

export const rotateOldLogFiles = () => {
  const oldLogFilePath = path.join(pubLogsFolderPath, `${APP_NAME}.old.json`);
  const compareLogFilePath = path.join(
    pubLogsFolderPath,
    `${APP_NAME}.compare.json`
  );

  if (!fs.existsSync(oldLogFilePath)) {
    return;
  }

  if (!fs.existsSync(compareLogFilePath)) {
    fs.copyFileSync(oldLogFilePath, compareLogFilePath);
    return;
  }

  const oldLogFileBuf = fs.readFileSync(oldLogFilePath);
  const compareLogFileBuf = fs.readFileSync(compareLogFilePath);

  if (oldLogFileBuf.equals(compareLogFileBuf)) {
    return;
  }

  composeRotatedOldLogFiles(compareLogFilePath);
  fs.copyFileSync(oldLogFilePath, compareLogFilePath);
};
