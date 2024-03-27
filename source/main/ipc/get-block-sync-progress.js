'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getBlockSyncProgressChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getBlockSyncProgressChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_BLOCK_SYNC_PROGRESS_CHANNEL
);
//# sourceMappingURL=get-block-sync-progress.js.map
