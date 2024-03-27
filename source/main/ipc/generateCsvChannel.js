'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleRewardsCsvRequests = exports.generateCsvChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const csv_stringify_1 = __importDefault(require('csv-stringify'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.generateCsvChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_CSV_CHANNEL
);
const handleRewardsCsvRequests = () => {
  exports.generateCsvChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        const { fileContent, filePath } = request;
        (0, csv_stringify_1.default)(fileContent, (csvErr, output) => {
          if (csvErr) {
            return reject(csvErr);
          }
          return fs_1.default.writeFile(filePath, output, (fileErr) => {
            if (fileErr) {
              return reject(fileErr);
            }
            return resolve();
          });
        });
      })
  );
};
exports.handleRewardsCsvRequests = handleRewardsCsvRequests;
//# sourceMappingURL=generateCsvChannel.js.map
