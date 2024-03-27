'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.exportWalletsChannel = exports.setCachedCardanoStatusChannel = exports.getCachedCardanoStatusChannel = exports.cardanoFaultInjectionChannel = exports.cardanoStateChangeChannel = exports.cardanoAwaitUpdateChannel = exports.cardanoTlsConfigChannel = exports.cardanoRestartChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.cardanoRestartChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CARDANO_RESTART_CHANNEL
);
exports.cardanoTlsConfigChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CARDANO_TLS_CONFIG_CHANNEL
);
exports.cardanoAwaitUpdateChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CARDANO_AWAIT_UPDATE_CHANNEL
);
exports.cardanoStateChangeChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CARDANO_STATE_CHANNEL
);
exports.cardanoFaultInjectionChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CARDANO_FAULT_INJECTION_CHANNEL
);
exports.getCachedCardanoStatusChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_CACHED_CARDANO_STATUS_CHANNEL
);
exports.setCachedCardanoStatusChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.SET_CACHED_CARDANO_STATUS_CHANNEL
);
exports.exportWalletsChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.EXPORT_WALLETS_CHANNEL
);
//# sourceMappingURL=cardano.ipc.js.map
