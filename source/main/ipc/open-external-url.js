'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.openExternalUrlChannel = void 0;
const electron_1 = require('electron');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.openExternalUrlChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.OPEN_EXTERNAL_URL_CHANNEL
);
exports.openExternalUrlChannel.onReceive((url) =>
  electron_1.shell.openExternal(url) ? Promise.resolve() : Promise.reject()
);
//# sourceMappingURL=open-external-url.js.map
