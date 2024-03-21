'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.openLocalDirectoryChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.openLocalDirectoryChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.OPEN_LOCAL_DIRECTORY_CHANNEL
);
//# sourceMappingURL=open-local-directory.js.map
