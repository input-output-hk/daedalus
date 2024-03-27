'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.saveQRCodeImageRequests = exports.saveQRCodeImageChannel = void 0;
const fs_1 = __importDefault(require('fs'));
const qr_image_1 = __importDefault(require('qr-image'));
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.saveQRCodeImageChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GENERATE_QRCODE_CHANNEL
);
const saveQRCodeImageRequests = () => {
  exports.saveQRCodeImageChannel.onReceive(
    (request) =>
      new Promise((resolve, reject) => {
        const { address, filePath } = request;
        try {
          const qrCodeImage = qr_image_1.default.image(address, {
            type: 'png',
            size: 10,
            ec_level: 'L',
          });
          const writeStream = fs_1.default.createWriteStream(filePath);
          qrCodeImage.pipe(writeStream);
          writeStream.on('close', resolve);
          writeStream.on('error', reject);
        } catch (error) {
          reject(error);
        }
      })
  );
};
exports.saveQRCodeImageRequests = saveQRCodeImageRequests;
//# sourceMappingURL=saveQRCodeImageChannel.js.map
