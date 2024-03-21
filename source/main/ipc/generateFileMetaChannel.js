'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleFileMetaRequests = exports.generateFileMetaChannel = void 0;
const mime_types_1 = __importDefault(require('mime-types'));
const path_1 = __importDefault(require('path'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.generateFileMetaChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_FILE_META_CHANNEL
);
const handleFileMetaRequests = () => {
  exports.generateFileMetaChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        const { filePath } = request;
        try {
          resolve({
            fileName: path_1.default.basename(filePath),
            fileType: mime_types_1.default.lookup(filePath),
          });
        } catch (err) {
          reject(err);
        }
      })
  );
};
exports.handleFileMetaRequests = handleFileMetaRequests;
//# sourceMappingURL=generateFileMetaChannel.js.map
