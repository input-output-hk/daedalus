'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.showUiPartChannel = exports.toggleUiPartChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.toggleUiPartChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.TOGGLE_UI_PART_CHANNEL
);
exports.showUiPartChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SHOW_UI_PART_CHANNEL
);
//# sourceMappingURL=control-ui-parts.js.map
