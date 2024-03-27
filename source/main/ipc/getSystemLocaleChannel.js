'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getSystemLocaleChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getSystemLocaleChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_SYSTEM_LOCALE_CHANNEL
);
//# sourceMappingURL=getSystemLocaleChannel.js.map
