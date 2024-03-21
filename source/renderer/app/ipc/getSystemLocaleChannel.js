'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getSystemLocaleChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.getSystemLocaleChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_SYSTEM_LOCALE_CHANNEL
);
//# sourceMappingURL=getSystemLocaleChannel.js.map
