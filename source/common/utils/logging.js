// @flow
import { pickBy } from 'lodash';
import type {
  FormatMessageContextParams,
  ConstructMessageBodyParams,
  MessageBody,
  ElectronLoggerMessage,
} from '../types/logging.types';

const DEFAULT_MESSAGE_BODY = {
  ns: ['daedalus'],
  data: {},
  app: ['daedalus'],
};

const stringifyMessageBody = (messageBody: MessageBody): string => {
  const isProd = process.env.NODE_ENV === 'production';
  const spacing = isProd ? 0 : 2;
  return JSON.stringify(messageBody, null, spacing);
};

export const filterLogData = (data: Object): Object => {
  const sensitiveData = [
    'spendingPassword', 'oldPassword', 'newPassword',
    'mnemonic', 'recoveryPhrase', 'passphrase', 'password',
  ];
  return pickBy(data, (value, key) => {
    if (sensitiveData.includes(key)) { return false; }
    return true;
  });
};

export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) => (
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2)
);

export const formatContext = (context: FormatMessageContextParams): string => {
  const { appName, electronProcess, level, network } = context;
  return `[${appName}.*${network}*:${level}:${electronProcess}]`;
};

export const formatMessageTime = (date: Date): string => {
  const [year, time] = date.toISOString().split('T');
  return `[${year} ${time.slice(0, -1)} UTC]`;
};

export const constructMessageBody = (bodyData: ConstructMessageBodyParams): MessageBody => {
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
    ns: [
      'daedalus',
      `v${version}`,
      `*${network}*`,
    ],
    data,
    msg,
    pid: '',
    sev: level,
    thread: '',
  };

  const messageTime: string = formatMessageTime(loggerMessage.date);
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);

  return `${context} ${messageTime}\n${stringifyMessageBody(messageBody)}`;
};
