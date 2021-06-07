// @flow
import { formatContext } from '../../../common/utils/logging';
import type {
  FormatMessageContextParams,
  Logger,
  LoggingLevel,
} from '../../../common/types/logging.types';

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
  data: ?Object
) => {
  const args = [
    formatContext({ ...messageContext, level }),
    {
      message,
      // The algorithm used to serialize objects sent over IPC on electron v9+
      // does not support sending Functions, Promises, WeakMaps, WeakSets,
      // or objects containing any such values.
      // https://github.com/electron/electron/blob/main/docs/breaking-changes.md#behavior-changed-values-sent-over-ipc-are-now-serialized-with-structured-clone-algorithm
      data: JSON.parse(JSON.stringify(data)),
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
