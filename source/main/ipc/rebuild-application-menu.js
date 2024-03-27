'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.rebuildApplicationMenu = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.rebuildApplicationMenu = new MainIpcChannel_1.MainIpcChannel(
  api_1.REBUILD_APP_MENU_CHANNEL
);
//# sourceMappingURL=rebuild-application-menu.js.map
