'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const electron_1 = require('electron');
exports.default = (window) => {
  electron_1.ipcMain.on(
    'resize-window',
    (event, { width, height, animate }) => {
      if (event.sender !== window.webContents) return;
      window.setSize(width, height, animate);
    }
  );
};
//# sourceMappingURL=resize-window.js.map
