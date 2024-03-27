'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getDiskSpaceStatusChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.getDiskSpaceStatusChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_DISK_SPACE_STATUS_CHANNEL
);
//# sourceMappingURL=getDiskSpaceChannel.js.map
