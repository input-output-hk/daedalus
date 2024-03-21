'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getRecoveryWalletIdChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getRecoveryWalletIdChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_WASM_BINARY_CHANNEL
);
//# sourceMappingURL=getRecoveryWalletIdChannel.js.map
