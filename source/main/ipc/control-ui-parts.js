'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.toggleUiPartChannel = exports.showUiPartChannel = void 0;
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.showUiPartChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.SHOW_UI_PART_CHANNEL
);
exports.toggleUiPartChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.TOGGLE_UI_PART_CHANNEL
);
//# sourceMappingURL=control-ui-parts.js.map
