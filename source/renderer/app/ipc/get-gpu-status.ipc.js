'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getGPUStatusChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
exports.getGPUStatusChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_GPU_STATUS_CHANNEL
);
//# sourceMappingURL=get-gpu-status.ipc.js.map
