// @flow
import { formatContext } from '../../../common/utils/logging';
import type {
  FormatMessageContextParams,
  Logger,
} from '../../../common/types/logging.types';

const log = global.electronLog;
const { environment } = global;

const appName = 'daedalus';
const electronProcess = 'ipcRenderer';
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
