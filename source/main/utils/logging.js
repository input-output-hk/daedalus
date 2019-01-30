// @flow
import log from 'electron-log';

const prefixProcessType = (str: string) => '[main] ' + str;

const logToLevel = (level) => (message: string) => log[level](prefixProcessType(message));

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
