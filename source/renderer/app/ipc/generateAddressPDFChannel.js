'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateAddressPDFChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.generateAddressPDFChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_ADDRESS_PDF_CHANNEL
);
//# sourceMappingURL=generateAddressPDFChannel.js.map
