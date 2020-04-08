// @flow
import { formatContext } from '../../../common/utils/logging';
import type {
  FormatMessageContextParams,
  Logger,
  LoggingLevel,
} from '../../../common/types/logging.types';
import { electronLogChannel } from '../ipc/logger-channel';

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

const logToLevel = (level: LoggingLevel) => (
  message: string,
  data: ?Object
) => {
  const args = [
    formatContext({ ...messageContext, level }),
    {
      message,
      data,
      environmentData,
    },
  ];
  electronLogChannel.send({ level, args });
};

export const logger: Logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
