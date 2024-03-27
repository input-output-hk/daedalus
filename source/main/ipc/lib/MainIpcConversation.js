'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.MainIpcConversation = void 0;
const electron_1 = require('electron');
const IpcConversation_1 = require('../../../common/ipc/lib/IpcConversation');
/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
class MainIpcConversation extends IpcConversation_1.IpcConversation {
  async send(message, sender, receiver = electron_1.ipcMain) {
    return super.request(message, sender, receiver);
  }
  async request(message, sender, receiver = electron_1.ipcMain) {
    return super.request(message, sender, receiver);
  }
  onReceive(handler, receiver = electron_1.ipcMain) {
    super.onRequest(handler, receiver);
  }
  onRequest(handler, receiver = electron_1.ipcMain) {
    super.onRequest(handler, receiver);
  }
}
exports.MainIpcConversation = MainIpcConversation;
//# sourceMappingURL=MainIpcConversation.js.map
