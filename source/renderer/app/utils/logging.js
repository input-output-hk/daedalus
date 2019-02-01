// @flow
import { formatContext } from '../../../common/utils/logging';

const log = global.electronLog;
const environment = global.environment;

const appName = 'daedalus';
const electronProcess = 'ipcRenderer';
const { network, os, platformVersion, version } = environment;

const messageContext = {
  appName,
  electronProcess,
  network,
};

const environmentData = {
  network,
  os,
  platformVersion,
  version
};

const logToLevel = (level: string) => (message: string, data: ?Object) => (
  log[level](formatContext({ ...messageContext, level }), { message, data, environmentData })
);

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
