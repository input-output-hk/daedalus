import path from 'path';
import { app, BrowserWindow, ipcMain, Menu, shell } from 'electron';
import environment from '../../common/environment';
import { runtimeFolderPath } from '../config';

export const createAboutWindow = () => {
  // Only really terminate about window when whole app is closed
  // otherwise keep it in the background so we can quickly reveal it
  let terminateAboutWindow = false;
  app.on('before-quit', () => {
    terminateAboutWindow = true;
  });

  const width = 640;
  const height = 486;
  const params = {
    fullscreenable: false,
    show: false,
    width,
    height,
  };
  if (process.platform === 'linux') {
    params.icon = path.join(runtimeFolderPath, 'icon.png');
  }

  // Load About window but keep it hidden
  const window = new BrowserWindow(params);

  // Prevent resize about window
  window.setMinimumSize(width, height);
  window.setMaximumSize(width, height);
  // Set default title
  window.setTitle('About Daedalus');

  // Update about window title when translation is ready
  ipcMain.on('about-window-title', (event, title) => {
    if (window) window.setTitle(title);
  });

  // IPC endpoint to reload about window (e.g: for updating displayed language)
  ipcMain.on('reload-about-window', (event) => {
    // Check that the about window exists but is not the sender of the ipc message!
    // Otherwise it endlessly re-loads itself.
    if (window && event.sender !== window.webContents) {
      window.reload();
    }
  });

  // Load the url for the window
  // points to `webpack-dev-server` in development
  // points to `index.html` in production
  window.loadURL(`file://${__dirname}/../renderer/index.html?window=about`);

  // Prevent native window title changes (we are handling these via IPC)
  window.on('page-title-updated', event => { event.preventDefault(); });

  // Prevent direct link navigation in electron window -> open in default browser
  window.webContents.on('will-navigate', (e, url) => {
    e.preventDefault();
    const history = e.sender.history;
    if (history.length && url !== history[history.length - 1]) {
      shell.openExternal(url);
    }
  });

  // Add 'inspect element' to context menu in dev modes
  window.webContents.on('context-menu', (e, props) => {
    const contextMenuOptions = [];

    if (environment.isDev() || environment.isTest()) {
      const { x, y } = props;
      contextMenuOptions.push({
        label: 'Inspect element',
        click() {
          window.inspectElement(x, y);
        }
      });
    }
    Menu.buildFromTemplate(contextMenuOptions).popup(window);
  });

  window.on('close', (e) => {
    if (terminateAboutWindow) {
      // The user is quitting the app
      app.quit();
    } else {
      // The user only closed the about window (so let's hide it)
      e.preventDefault();
      window.hide();
    }
  });

  return window;
};
