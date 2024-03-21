'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.logger = void 0;
const logging_1 = require('../../../common/utils/logging');
// @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'typ... Remove this comment to see the full error message
const { environment, electronLog } = global;
const appName = 'daedalus';
const electronProcess = 'ipcRenderer';
const { network, os, platformVersion, version } = environment;
const messageContext = {
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
const logToLevel = (level) => (message, data) => {
  const args = [
    (0, logging_1.formatContext)({ ...messageContext, level }),
    {
      message,
      data,
      environmentData,
    },
  ];
  electronLog[level](...args);
};
exports.logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
//# sourceMappingURL=logging.js.map
