// @flow
import { isEmpty } from 'lodash';
import type {
  FormatMessageContextParams,
  ConstructMessageBodyParams,
  MessageBody
} from '../types/logging.types';

const DEFAULT_MESSAGE_BODY: MessageBody = {
  at: '',
  env: '',
  ns: [
    'daedalus',
  ],
  data: {},
  app: [
    'daedalus'
  ],
  msg: '',
  pid: '',
  sev: '',
  thread: ''
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
  const { data = {} } = bodyData;
  let messageBody = { ...bodyData, data };

  if (typeof data === 'object' && !isEmpty(data)) {
    messageBody = { ...messageBody, data };
  }

  if (typeof data === 'string' && data !== ' ') {
    messageBody = { ...messageBody, data: { response: data } };
  }

  return { ...DEFAULT_MESSAGE_BODY, ...messageBody };
};

export const formatMessage = (message: Object): string => {
  const at = message.date.toISOString();
  const [context, messageData] = message.data;
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
  };

  const messageTime: string = formatMessageTime(message.date);
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);

  return `${context} ${messageTime} ${stringifyData(messageBody)}`;
};
