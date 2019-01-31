// @flow
import { formatContext } from '../../../common/utils/logging';

const log = global.electronLog;
const environment = global.environment;

const appName = 'daedalus';
const electronProcess = 'ipcRenderer';
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
