// @flow
import log from 'electron-log-daedalus';
import { environment } from '../environment';
import { formatContext } from '../../common/utils/logging';
import type { FormatMessageContextParams } from '../../common/types/logging.types';

export type Logger = {
  debug: (string, ?Object) => void,
  info: (string, ?Object) => void,
  error: (string, ?Object) => void,
  warn: (string, ?Object) => void,
};

const appName = 'daedalus';
const electronProcess = 'ipcMain';
const { network, os, platformVersion, version } = environment;

const messageContext: FormatMessageContextParams = {
  appName,
  electronProcess,
  network,
  level: '',
};

const environmentData = {
  network,
  os,
  platformVersion,
  version,
};

const logToLevel = (level: string) => (message: string, data: ?Object) =>
  log[level](formatContext({ ...messageContext, level }), {
    message,
    data,
    environmentData,
  });

export const logger: Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
