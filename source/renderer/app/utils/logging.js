// @flow
const log = global.electronLog;
const environment = global.environment;

const { appName } = log;
const { network } = environment;

const prefixContext = (level: string, message: string): string => (
  `[${appName}.*${network}*:${level}:ipcRenderer] ${message}`
);

const logToLevel = (level) => (message: string) => log[level](prefixContext(level, message));

export const Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
