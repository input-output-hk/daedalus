'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.RendererIpcConversation = void 0;
const IpcConversation_1 = require('../../../../common/ipc/lib/IpcConversation');
/**
 * Subclass of IpcChannel that uses ipcRenderer to send and receive messages.
 */
class RendererIpcConversation extends IpcConversation_1.IpcConversation {
  async send(
    message,
    sender = global.ipcRenderer,
    receiver = global.ipcRenderer
  ) {
    return super.request(message, sender, receiver);
  }
  async request(
    message,
    sender = global.ipcRenderer,
    receiver = global.ipcRenderer
  ) {
    return super.request(message, sender, receiver);
  }
  onReceive(handler, receiver = global.ipcRenderer) {
    super.onRequest(handler, receiver);
  }
  onRequest(handler, receiver = global.ipcRenderer) {
    super.onRequest(handler, receiver);
  }
}
exports.RendererIpcConversation = RendererIpcConversation;
//# sourceMappingURL=RendererIpcConversation.js.map
