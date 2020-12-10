// @flow
import omitDeep from 'omit-deep-lodash';
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
    'votingKey',
    'signature',
    'accountPublicKey',
    'extendedPublicKey',
    'publicKeyHex',
    'chainCodeHex',
    'signedTransactionBlob',
    'withdrawal',
  ];
  return omitDeep(data, ...sensitiveData);
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
