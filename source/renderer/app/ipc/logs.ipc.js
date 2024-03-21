'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.downloadLogsChannel = exports.compressLogsChannel = exports.getLogsChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getLogsChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_LOGS_CHANNEL
);
exports.compressLogsChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.COMPRESS_LOGS_CHANNEL
);
exports.downloadLogsChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.DOWNLOAD_LOGS_CHANNEL
);
//# sourceMappingURL=logs.ipc.js.map
