'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.downloadLogsChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.downloadLogsChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.DOWNLOAD_LOGS_CHANNEL
);
exports.default = () => {
  exports.downloadLogsChannel.onRequest((request) => {
    const { compressedLogsFilePath, destinationPath } = request;
    if (!fs_1.default.existsSync(compressedLogsFilePath)) {
      return Promise.reject(new Error('File does not exist'));
    }
    const file = fs_1.default.readFileSync(compressedLogsFilePath);
    fs_1.default.writeFileSync(destinationPath, file);
    return Promise.resolve();
  });
};
//# sourceMappingURL=download-logs.js.map
