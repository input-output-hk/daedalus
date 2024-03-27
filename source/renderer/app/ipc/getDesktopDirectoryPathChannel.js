'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getDesktopDirectoryPathChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.getDesktopDirectoryPathChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_DESKTOP_DIRECTORY_PATH_CHANNEL
);
//# sourceMappingURL=getDesktopDirectoryPathChannel.js.map
