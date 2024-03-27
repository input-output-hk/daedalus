'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleFileDialogRequests = exports.showSaveDialogChannel = exports.showOpenDialogChannel = void 0;
const electron_1 = require('electron');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
exports.showOpenDialogChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.SHOW_OPEN_DIALOG_CHANNEL
);
exports.showSaveDialogChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.SHOW_SAVE_DIALOG_CHANNEL
);
const handleFileDialogRequests = (window) => {
  exports.showOpenDialogChannel.onReceive((request) =>
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'FileDialogRequestParams' is not ... Remove this comment to see the full error message
    electron_1.dialog.showOpenDialog(window, request)
  );
  exports.showSaveDialogChannel.onReceive((request) =>
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'FileDialogRequestParams' is not ... Remove this comment to see the full error message
    electron_1.dialog.showSaveDialog(window, request)
  );
};
exports.handleFileDialogRequests = handleFileDialogRequests;
//# sourceMappingURL=show-file-dialog-channels.js.map
