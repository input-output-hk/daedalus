'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getStateDirectoryPathChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.getStateDirectoryPathChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_STATE_DIRECTORY_PATH_CHANNEL
);
//# sourceMappingURL=getStateDirectoryPathChannel.js.map
