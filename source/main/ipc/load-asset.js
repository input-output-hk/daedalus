'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const fs_1 = __importDefault(require('fs'));
const path_1 = __importDefault(require('path'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
const loadAssetChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.LOAD_ASSET_CHANNEL
);
exports.default = () => {
  loadAssetChannel.onRequest((request) => {
    const asset = path_1.default.resolve(
      __dirname,
      `../renderer/${request.fileName}`
    );
    return new Promise((resolve, reject) => {
      fs_1.default.readFile(asset, 'base64', (error, data) => {
        if (error) {
          reject(error);
        } else {
          resolve(data);
        }
      });
    });
  });
};
//# sourceMappingURL=load-asset.js.map
