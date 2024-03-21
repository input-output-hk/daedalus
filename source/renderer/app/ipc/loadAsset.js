'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.loadAssetChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.loadAssetChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.LOAD_ASSET_CHANNEL
);
//# sourceMappingURL=loadAsset.js.map
