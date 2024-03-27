'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.rebuildApplicationMenu = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
exports.rebuildApplicationMenu = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.REBUILD_APP_MENU_CHANNEL
);
//# sourceMappingURL=rebuild-application-menu.js.map
