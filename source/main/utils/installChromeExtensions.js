'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.installChromeExtensions = void 0;
const installChromeExtensions = async (isDev) => {
  if (isDev) {
    const {
      default: installExtension,
      REACT_DEVELOPER_TOOLS,
    } = require('electron-devtools-installer');
    // eslint-disable-line global-require
    const { app } = require('electron');
    const extensions = [REACT_DEVELOPER_TOOLS];
    const options = {
      loadExtensionOptions: {
        allowFileAccess: true,
      },
    };
    try {
      await app.whenReady();
      await installExtension(extensions, options);
    } catch (e) {} // eslint-disable-line
  }
};
exports.installChromeExtensions = installChromeExtensions;
//# sourceMappingURL=installChromeExtensions.js.map
