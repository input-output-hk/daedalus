// @flow
import { isEmpty } from 'lodash';
import type { MessageContext } from '../types/logging.types';

export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) => (
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2)
);

export const formatContext = (context: MessageContext): string => {
  const { appName, electronProcess, level, network } = context;
  return `[${appName}.*${network}*:${level}:${electronProcess}]`;
};

export const formatMessage = (message: Object) => {
  // message data
  const date = message.date.toISOString();
  const [year, time] = date.split('T');
  const [context, messageData] = message.data;
  const { message: msg, data, environmentData } = messageData;
  const { network, os, platformVersion, version } = environmentData;

  let messageBody = {
    at: date,
    env: `${network}:${os}:${platformVersion}`,
    ns: [
      'daedalus',
      `${version}`,
      `*${network}*`,
    ],
    data: {},
    app: [
      'daedalus'
    ],
    msg,
    pid: '',
    sev: '',
    thread: ''
  };

  if (typeof data === 'object' && !isEmpty(data)) {
    messageBody = { ...messageBody, data };
  }

  if (typeof data === 'string' && data !== ' ') {
    messageBody = { ...messageBody, data: { response: data } };
  }

  console.log(`${context} [${year} ${time.slice(0, -1)} UTC]`);
  console.log(stringifyData(messageBody));
};
