'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.toggleRTSFlagsModeChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.toggleRTSFlagsModeChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.TOGGLE_RTS_FLAGS_MODE_CHANNEL
);
//# sourceMappingURL=toggleRTSFlagsModeChannel.js.map
