'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.introspectAddressChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.introspectAddressChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.INTROSPECT_ADDRESS_CHANNEL
);
//# sourceMappingURL=introspect-address.js.map
