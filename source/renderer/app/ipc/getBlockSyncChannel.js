'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getBlockSyncProgressChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.getBlockSyncProgressChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_BLOCK_SYNC_PROGRESS_CHANNEL
);
//# sourceMappingURL=getBlockSyncChannel.js.map
