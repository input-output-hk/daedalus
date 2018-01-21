import path from 'path';
import { app, BrowserWindow, globalShortcut, ipcMain, Menu } from 'electron';
import environment from '../../common/environment';
import ipcApi from '../ipc-api';

export const createMainWindow = () => {
  // Construct new BrowserWindow
  const window = new BrowserWindow({
    show: false,
    width: 1150,
    height: 870
  });

  window.setMinimumSize(900, 600);

  // Initialize our ipc api methods that can be called by the render processes
  ipcApi({ window });

  // Provide render process with an api to resize the main window
  ipcMain.on('resize-window', (event, { width, height, animate }) => {
    if (event.sender !== window.webContents) return;
    window.setSize(width, height, animate);
  });

  const port = environment.ELECTRON_WEBPACK_WDS_PORT;

  // Set url for the window
  // points to `webpack-dev-server` in development
  // points to `index.html` in production
  const url = environment.isDev()
    ? `http://localhost:${port}`
    : `file://${path.resolve(__dirname, '../renderer')}/index.html`;

  if (environment.isDev()) {
    window.webContents.openDevTools();
    if (!environment.isTest()) {
      // Focus the main window after dev tools opened
      window.webContents.on('devtools-opened', () => {
        window.focus();
        setImmediate(() => {
          window.focus();
        });
      });
    }
  }

  window.loadURL(url);
  window.on('page-title-updated', event => { event.preventDefault(); });
  window.setTitle(`Daedalus (${environment.DAEDALUS_VERSION || 'dev'})`);

  window.webContents.on('context-menu', (e, props) => {
    const contextMenuOptions = [
      { label: 'Copy', accelerator: 'CmdOrCtrl+C', selector: 'copy:' },
      { label: 'Paste', accelerator: 'CmdOrCtrl+V', selector: 'paste:' },
    ];

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

  window.webContents.on('did-finish-load', () => {
    if (environment.isTest()) {
      window.showInactive(); // show without focusing the window
    } else {
      window.show(); // show also focuses the window
    }
  });

  window.on('closed', () => {
    app.quit();
  });

  return window;
};
