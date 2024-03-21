'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.exportWalletsChannel = exports.setCachedCardanoStatusChannel = exports.getCachedCardanoStatusChannel = exports.cardanoFaultInjectionChannel = exports.awaitUpdateChannel = exports.cardanoStateChangeChannel = exports.restartCardanoNodeChannel = exports.cardanoTlsConfigChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.cardanoTlsConfigChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CARDANO_TLS_CONFIG_CHANNEL
);
exports.restartCardanoNodeChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CARDANO_RESTART_CHANNEL
);
exports.cardanoStateChangeChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CARDANO_STATE_CHANNEL
);
exports.awaitUpdateChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CARDANO_AWAIT_UPDATE_CHANNEL
);
exports.cardanoFaultInjectionChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.CARDANO_FAULT_INJECTION_CHANNEL
);
exports.getCachedCardanoStatusChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_CACHED_CARDANO_STATUS_CHANNEL
);
exports.setCachedCardanoStatusChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SET_CACHED_CARDANO_STATUS_CHANNEL
);
exports.exportWalletsChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.EXPORT_WALLETS_CHANNEL
);
//# sourceMappingURL=cardano.ipc.js.map
