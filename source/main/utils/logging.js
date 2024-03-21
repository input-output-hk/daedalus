'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.logger = void 0;
const electron_log_daedalus_1 = __importDefault(
  require('electron-log-daedalus')
);
const environment_1 = require('../environment');
const logging_1 = require('../../common/utils/logging');
const helper_1 = require('../../common/utils/helper');
const appName = 'daedalus';
const electronProcess = 'ipcMain';
const { network, os, platformVersion, version } = environment_1.environment;
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
const logToLevel = (level) => (message, data) =>
  electron_log_daedalus_1.default[level](
    (0, logging_1.formatContext)({ ...messageContext, level }),
    {
      message,
      data: (0, helper_1.toJS)(data),
      environmentData,
    }
  );
exports.logger = {
  debug: logToLevel('debug'),
  info: logToLevel('info'),
  error: logToLevel('error'),
  warn: logToLevel('warn'),
};
//# sourceMappingURL=logging.js.map
