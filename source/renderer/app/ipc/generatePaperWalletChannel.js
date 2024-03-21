'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generatePaperWalletChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.generatePaperWalletChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GENERATE_PAPER_WALLET_CHANNEL
);
//# sourceMappingURL=generatePaperWalletChannel.js.map
