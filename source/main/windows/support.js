import path from 'path';
import { BrowserWindow } from 'electron';
import environment from '../../common/environment';
import { runtimeFolderPath } from '../config';
import RendererErrorHandler from '../utils/rendererErrorHandler';

const rendererErrorHandler = new RendererErrorHandler();

export const createSupportWindow = (unsetSupportWindow) => {
  const windowOptions = {
    show: false,
    width: 375,
    height: 598,
    resizable: false,
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

  const title = 'Support';
  window.setTitle(title);

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
