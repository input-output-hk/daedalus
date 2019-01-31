// @flow
import log from 'electron-log';
import { environment } from '../environment';
import { formatContext, formatMessage } from '../../common/utils/logging';

log.transports.console = formatMessage;
log.transports.file = formatMessage;

const appName = 'daedalus';
const electronProcess = 'ipcMain';
const { network } = environment;

const messageContext = {
  appName,
  electronProcess,
  network,
};

const logToLevel = (level: string) => (message: string, data: ?Object) => (
  log[level](formatContext({ ...messageContext, level }), message, data)
);

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
