import { formatContext } from '../../../common/utils/logging';
import type {
  FormatMessageContextParams,
  Logger,
  LoggingLevel,
} from '../../../common/types/logging.types';

// @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'typ... Remove this comment to see the full error message
const { environment, electronLog } = global;
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

const logToLevel = (level: LoggingLevel) => (
  message: string,
  data: Record<string, any> | null | undefined
) => {
  const args = [
    formatContext({ ...messageContext, level }),
    {
      message,
      data,
      environmentData,
    },
  ];
  electronLog[level](...args);
};

export const logger: Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
