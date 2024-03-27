'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.MainIpcChannel = void 0;
const electron_1 = require('electron');
const IpcChannel_1 = require('../../../common/ipc/lib/IpcChannel');
/**
 * Subclass of IpcChannel that uses ipcMain to receive messages.
 */
class MainIpcChannel extends IpcChannel_1.IpcChannel {
  async send(message, sender, receiver = electron_1.ipcMain) {
    return super.send(message, sender, receiver);
  }
  async request(message, sender, receiver = electron_1.ipcMain) {
    return super.request(message, sender, receiver);
  }
  onReceive(handler, receiver = electron_1.ipcMain) {
    super.onReceive(handler, receiver);
  }
  onRequest(handler, receiver = electron_1.ipcMain) {
    super.onRequest(handler, receiver);
  }
}
exports.MainIpcChannel = MainIpcChannel;
//# sourceMappingURL=MainIpcChannel.js.map
