'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleBugReportRequests = exports.bugReportRequestChannel = void 0;
const http_1 = __importDefault(require('http'));
const form_data_1 = __importDefault(require('form-data/lib/form_data'));
const fs_1 = __importDefault(require('fs'));
const files_1 = require('../../common/utils/files');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
const logging_1 = require('../utils/logging');
/* eslint-disable consistent-return */
exports.bugReportRequestChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.SUBMIT_BUG_REPORT_REQUEST_CHANNEL
);
const handleBugReportRequests = () => {
  exports.bugReportRequestChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        logging_1.logger.info('bugReportRequestChannel::onReceive', {
          request,
        });
        const { httpOptions, requestPayload } = request;
        const options = Object.assign({}, httpOptions);
        const payload = Object.assign({}, requestPayload);
        // Prepare multipart/form-data
        const formData = new form_data_1.default();
        formData.append('payload', JSON.stringify(payload));
        // prepare file stream (attachment)
        // @ts-ignore ts-migrate(2339) FIXME: Property 'compressedLogsFilePath' does not exist o... Remove this comment to see the full error message
        if (payload.compressedLogsFilePath) {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'compressedLogsFilePath' does not exist o... Remove this comment to see the full error message
          const stream = fs_1.default.createReadStream(
            payload.compressedLogsFilePath
          );
          const fileName = (0, files_1.extractFileNameFromPath)(
            // @ts-ignore ts-migrate(2339) FIXME: Property 'compressedLogsFilePath' does not exist o... Remove this comment to see the full error message
            payload.compressedLogsFilePath
          );
          formData.append(fileName, stream);
        }
        options.headers = formData.getHeaders();
        logging_1.logger.info('Sending bug report request with options', {
          options,
        });
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BugReportRequestHttpOptions' is ... Remove this comment to see the full error message
        const httpRequest = http_1.default.request(options);
        httpRequest.on('response', (response) => {
          if (response.statusCode !== 200) {
            return reject();
          }
          response.on('data', () => {});
          response.on('error', (error) => {
            reject(error);
          });
          response.on('end', () => {
            resolve();
          });
        });
        httpRequest.on('error', (error) => reject(error));
        // Attach form-data and trigger the request
        formData.pipe(httpRequest);
      })
  );
};
exports.handleBugReportRequests = handleBugReportRequests;
//# sourceMappingURL=bugReportRequestChannel.js.map
