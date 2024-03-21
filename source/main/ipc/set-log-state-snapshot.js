'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.setStateSnapshotLogChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.setStateSnapshotLogChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.SET_STATE_SNAPSHOT_LOG_CHANNEL
);
//# sourceMappingURL=set-log-state-snapshot.js.map
