'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.manageAppUpdateChannel = void 0;
const api_1 = require('../../../common/ipc/api');
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
exports.manageAppUpdateChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.MANAGE_APP_UPDATE
);
//# sourceMappingURL=manageAppUpdateChannel.js.map
