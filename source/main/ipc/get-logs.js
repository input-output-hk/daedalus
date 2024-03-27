'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getLogsChannel = void 0;
const lodash_1 = require('lodash');
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const config_1 = require('../config');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getLogsChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_LOGS_CHANNEL
);
const isOldDaedalusLog = (fileName) => {
  const appLogNamePosition = fileName.indexOf(config_1.ALLOWED_LOGS[0]);
  // if fileName doesn't start with AppLog filename, it's not old daedalus log
  if (appLogNamePosition !== 0) {
    return false;
  }
  // extract subsequent string attached to log file name
  const fileNamePostfix = fileName.substring(config_1.ALLOWED_LOGS[0].length);
  // if extracted string is in -{datetimestamp} format, it's old daedalus log
  if (/^-\d{14}$/.test(fileNamePostfix)) {
    return true;
  }
  return false;
};
const isFileAllowed = (fileName) =>
  (0, lodash_1.includes)(config_1.ALLOWED_LOGS, fileName) ||
  isOldDaedalusLog(fileName);
const isFileNodeLog = (fileName, nodeLogsIncluded) =>
  config_1.ALLOWED_NODE_LOGS.test(fileName) &&
  nodeLogsIncluded < config_1.MAX_NODE_LOGS_ALLOWED;
const isFileWalletLog = (fileName, walletLogsIncluded) =>
  config_1.ALLOWED_WALLET_LOGS.test(fileName) &&
  walletLogsIncluded < config_1.MAX_WALLET_LOGS_ALLOWED;
const isFileLauncherLog = (fileName, nodeLogsIncluded) =>
  config_1.ALLOWED_LAUNCHER_LOGS.test(fileName) &&
  nodeLogsIncluded < config_1.MAX_LAUNCHER_LOGS_ALLOWED;
exports.default = () => {
  exports.getLogsChannel.onRequest(() => {
    // check if pub folder exists and create array of log file names
    const logFiles = [];
    if (fs_1.default.existsSync(config_1.pubLogsFolderPath)) {
      const files = fs_1.default
        .readdirSync(config_1.pubLogsFolderPath)
        .sort()
        .reverse();
      let nodeLogsIncluded = 0;
      let walletLogsIncluded = 0;
      let launcherLogsIncluded = 0;
      for (let i = 0; i < files.length; i++) {
        const currentFile = path_1.default.join(
          config_1.pubLogsFolderPath,
          files[i]
        );
        if (fs_1.default.statSync(currentFile).isFile()) {
          const fileName = path_1.default.basename(currentFile);
          if (isFileAllowed(fileName)) {
            logFiles.push(fileName);
          } else if (isFileNodeLog(fileName, nodeLogsIncluded)) {
            logFiles.push(fileName);
            nodeLogsIncluded++;
          } else if (isFileWalletLog(fileName, walletLogsIncluded)) {
            logFiles.push(fileName);
            walletLogsIncluded++;
          } else if (isFileLauncherLog(fileName, launcherLogsIncluded)) {
            logFiles.push(fileName);
            launcherLogsIncluded++;
          }
        }
      }
    }
    const logs = {
      path: config_1.pubLogsFolderPath,
      files: (0, lodash_1.sortBy)(logFiles, (log) => {
        // custom file sorting which enforces correct ordering (like in ALLOWED_LOGS)
        const nameSegments = log.split('.');
        return nameSegments.shift() + nameSegments.join('').length;
      }),
    };
    return Promise.resolve(logs);
  });
};
//# sourceMappingURL=get-logs.js.map
