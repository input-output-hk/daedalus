'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.showSaveDialogChannel = exports.showOpenDialogChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
exports.showOpenDialogChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SHOW_OPEN_DIALOG_CHANNEL
);
exports.showSaveDialogChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SHOW_SAVE_DIALOG_CHANNEL
);
//# sourceMappingURL=show-file-dialog-channels.js.map
