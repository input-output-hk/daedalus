'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getStateDirectoryPathChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getStateDirectoryPathChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_STATE_DIRECTORY_PATH_CHANNEL
);
//# sourceMappingURL=getStateDirectoryPathChannel.js.map
