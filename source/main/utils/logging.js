// @flow
import log from 'electron-log';
import { environment } from '../environment';
import { formatMessage } from '../../common/utils/logging';

log.transports.console = formatMessage;
log.transports.file = formatMessage;

const appName = 'daedalus';
const { network } = environment;
const electronProcess = 'ipcMain';

const formatContext = (level: string): string => `[${appName}.*${network}*:${level}:${electronProcess}]`;

const logToLevel = (level: string) => (message: string) => (
  log[level](formatContext(level), message)
);

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
