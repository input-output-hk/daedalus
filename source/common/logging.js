// @flow
import { isNodeEnvironment } from './environment';

// TODO: Expose log via preload script to renderer
const _nodeRequire = require;
const log = isNodeEnvironment ? _nodeRequire('electron-log') : global.electronLog;

const prefixProcessType = (str: string) => (isNodeEnvironment ? '[main] ' : '[renderer] ') + str;

const logToLevel = (level) => (message: string) => log[level](prefixProcessType(message));

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};

// ========== STRINGIFY =========

export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) => (
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2)
);

