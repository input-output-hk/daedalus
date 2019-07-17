// @flow
export const installChromeExtensions = async (isDev: boolean) => {
  if (isDev) {
    const {
      default: installer,
      REACT_DEVELOPER_TOOLS,
      REDUX_DEVTOOLS,
    } = require('electron-devtools-installer');

    const extensions = [REACT_DEVELOPER_TOOLS, REDUX_DEVTOOLS];
    const forceDownload = !!process.env.UPGRADE_EXTENSIONS;
    for (const extension of extensions) {
      try {
        await installer(extension, forceDownload);
      } catch (e) {} // eslint-disable-line
    }
  }
};
