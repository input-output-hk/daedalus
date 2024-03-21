'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.RendererIpcChannel = void 0;
const IpcChannel_1 = require('../../../../common/ipc/lib/IpcChannel');
/**
 * Subclass of IpcChannel that uses ipcRenderer to send and receive messages.
 */
class RendererIpcChannel extends IpcChannel_1.IpcChannel {
  async send(
    message,
    sender = global.ipcRenderer,
    receiver = global.ipcRenderer
  ) {
    return super.send(message, sender, receiver);
  }
  async request(
    message,
    sender = global.ipcRenderer,
    receiver = global.ipcRenderer
  ) {
    return super.request(message, sender, receiver);
  }
  onReceive(handler, receiver = global.ipcRenderer) {
    super.onReceive(handler, receiver);
  }
  onRequest(handler, receiver = global.ipcRenderer) {
    super.onRequest(handler, receiver);
  }
}
exports.RendererIpcChannel = RendererIpcChannel;
//# sourceMappingURL=RendererIpcChannel.js.map
