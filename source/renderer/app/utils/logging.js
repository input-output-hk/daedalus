// @flow
const log = global.electronLog;
const environment = global.environment;

const appName = 'daedalus';
const { network } = environment;
const electronProcess = 'ipcRenderer';

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
