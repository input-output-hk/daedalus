// @flow
export const installChromeExtensions = async (isDev: boolean) => {
  if (isDev) {
    const installer = require('electron-devtools-installer'); // eslint-disable-line global-require

    const extensions = ['REACT_DEVELOPER_TOOLS'];
    const forceDownload = !!process.env.UPGRADE_EXTENSIONS;
    for (const name of extensions) {
      try {
        await installer.default(installer[name], forceDownload);
      } catch (e) {} // eslint-disable-line
    }
  }
};
