import log from 'electron-log-daedalus';
import { environment } from '../environment';
import { formatContext } from '../../common/utils/logging';
import type {
  FormatMessageContextParams,
  Logger,
} from '../../common/types/logging.types';
import { toJS } from '../../common/utils/helper';

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

const logToLevel = (level: string) => (
  message: string,
  data: Record<string, any> | null | undefined
) =>
  log[level](formatContext({ ...messageContext, level }), {
    message,
    data: toJS(data),
    environmentData,
  });

export const logger: Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
