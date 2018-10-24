import path from 'path';
import { BrowserWindow } from 'electron';
import environment from '../../common/environment';
import { runtimeFolderPath } from '../config';
import RendererErrorHandler from '../utils/rendererErrorHandler';

const rendererErrorHandler = new RendererErrorHandler();

export const createSupportWindow = (unsetSupportWindow) => {
  const mainWindow = BrowserWindow.getFocusedWindow();
  const mainWindowPosition = mainWindow.getPosition();
  const mainWindowX = mainWindowPosition[0];
  const mainWindowY = mainWindowPosition[1];
  const mainWindowSize = mainWindow.getSize();
  const mainWindowWidth = mainWindowSize[0];
  const mainWindowHeight = mainWindowSize[1];

  const supportWindowWidth = 375;
  const supportWindowHeight = 598;
  const supportWindowX = mainWindowX + mainWindowWidth - supportWindowWidth - 10;
  const supportWindowY = mainWindowY + mainWindowHeight - supportWindowHeight - 10;

  const windowOptions = {
    show: false,
    frame: false,
    width: supportWindowWidth,
    height: supportWindowHeight,
    x: supportWindowX,
    y: supportWindowY,
    parent: mainWindow,
    resizable: false,
    transparent: true,
    webPreferences: {
      webviewTag: false,
    }
  };

  if (process.platform === 'linux') {
    windowOptions.icon = path.join(runtimeFolderPath, 'icon.png');
  }

  // Construct new BrowserWindow
  const window = new BrowserWindow(windowOptions);

  rendererErrorHandler.setup(window, createSupportWindow);

  if (environment.isDev()) {
    window.webContents.openDevTools();
    // Focus the main window after dev tools opened
    window.webContents.on('devtools-opened', () => {
      window.focus();
      setImmediate(() => {
        window.focus();
      });
    });
  }

  window.loadURL(`file://${__dirname}/../renderer/support.html`);
  window.on('page-title-updated', event => { event.preventDefault(); });

  window.webContents.on('did-finish-load', () => {
    if (environment.isTest()) {
      window.showInactive(); // show without focusing the window
    } else {
      window.show(); // show also focuses the window
    }
  });

  window.on('closed', () => {
    unsetSupportWindow && unsetSupportWindow();
  });

  window.webContents.on('did-fail-load', (err) => {
    rendererErrorHandler.onError('did-fail-load', err);
  });

  window.webContents.on('crashed', (err) => {
    rendererErrorHandler.onError('crashed', err);
  });

  return window;
};
