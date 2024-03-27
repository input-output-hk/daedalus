'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getGPUStatusChannel = void 0;
const electron_1 = require('electron');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.getGPUStatusChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_GPU_STATUS_CHANNEL
);
exports.default = () => {
  exports.getGPUStatusChannel.onRequest(() =>
    Promise.resolve(electron_1.app.getGPUFeatureStatus())
  );
};
//# sourceMappingURL=get-gpu-status.js.map
