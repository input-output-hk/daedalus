'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.electronStoreConversation = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcConversation_1 = require('./lib/RendererIpcConversation');
// RendererIpcConversation<Incoming, Outgoing>
exports.electronStoreConversation = new RendererIpcConversation_1.RendererIpcConversation(
  api_1.ELECTRON_STORE_CHANNEL
);
//# sourceMappingURL=electronStoreConversation.js.map
