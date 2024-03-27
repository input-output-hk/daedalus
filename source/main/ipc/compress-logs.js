'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.compressLogsChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const archiver_1 = __importDefault(require('archiver'));
const path_1 = __importDefault(require('path'));
const lodash_1 = require('lodash');
const config_1 = require('../config');
const logging_1 = require('../utils/logging');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.compressLogsChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.COMPRESS_LOGS_CHANNEL
);
exports.default = () => {
  exports.compressLogsChannel.onRequest(
    ({ logs, compressedFileName }) =>
      new Promise((resolve, reject) => {
        const outputPath = path_1.default.join(
          config_1.appLogsFolderPath,
          compressedFileName
        );
        const output = fs_1.default.createWriteStream(outputPath);
        const archive = (0, archiver_1.default)('zip', {
          zlib: {
            level: 9,
          }, // Sets the compression level
        });
        output.on('close', () => {
          logging_1.logger.debug('COMPRESS_LOGS.SUCCESS', {
            outputPath,
          });
          resolve(outputPath);
        });
        archive.on('error', (error) => {
          logging_1.logger.error('COMPRESS_LOGS.ERROR', {
            error,
          });
          reject(error);
        });
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.debug('COMPRESS_LOGS.START');
        // compress files
        const logFiles = (0, lodash_1.get)(logs, ['files'], []);
        for (let i = 0; i < logFiles.length; i++) {
          const stream = fs_1.default.readFileSync(
            path_1.default.join(config_1.pubLogsFolderPath, logFiles[i])
          );
          archive.append(stream, {
            name: logFiles[i],
          });
        }
        archive.finalize((error) => {
          if (error) {
            logging_1.logger.error('COMPRESS_LOGS.ERROR', {
              error,
            });
            reject(error);
          }
        });
        archive.pipe(output);
      })
  );
};
//# sourceMappingURL=compress-logs.js.map
