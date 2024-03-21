'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.saveQRCodeImageChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.saveQRCodeImageChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_QRCODE_CHANNEL
);
//# sourceMappingURL=saveQRCodeImageChannel.js.map
