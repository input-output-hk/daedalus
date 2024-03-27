'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.setStateSnapshotLogChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
exports.setStateSnapshotLogChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SET_STATE_SNAPSHOT_LOG_CHANNEL
);
//# sourceMappingURL=setStateSnapshotLogChannel.js.map
