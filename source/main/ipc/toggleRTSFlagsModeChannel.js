'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.toggleRTSFlagsModeChannel = void 0;
const api_1 = require('../../common/ipc/api');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
exports.toggleRTSFlagsModeChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.TOGGLE_RTS_FLAGS_MODE_CHANNEL
);
//# sourceMappingURL=toggleRTSFlagsModeChannel.js.map
