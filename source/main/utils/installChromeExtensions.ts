export const installChromeExtensions = async (isDev: boolean) => {
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
