'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.openExternalUrlChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
exports.openExternalUrlChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.OPEN_EXTERNAL_URL_CHANNEL
);
//# sourceMappingURL=open-external-url.js.map
