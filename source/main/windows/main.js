// @flow
import path from 'path';
import { app, BrowserWindow, ipcMain, Menu } from 'electron';
import { environment } from '../environment';
import ipcApi from '../ipc';
import RendererErrorHandler from '../utils/rendererErrorHandler';
import { getTranslation } from '../utils/getTranslation';
import { getContentMinimumSize } from '../utils/getContentMinimumSize';
import { launcherConfig } from '../config';

const rendererErrorHandler = new RendererErrorHandler();

const {
  isDev,
  isTest,
  buildLabel,
  isLinux,
  isBlankScreenFixActive,
} = environment;

const id = 'window';

const getWindowTitle = (locale: string): string => {
  const translations = require(`../locales/${locale}`);
  const translation = getTranslation(translations, id);
  let title = buildLabel;
  if (isBlankScreenFixActive)
    title += ` ${translation('title.blankScreenFix')}`;
  return title;
};

type WindowOptionsType = {
  show: boolean,
  width: number,
  height: number,
  webPreferences: {
    nodeIntegration: boolean,
    webviewTag: boolean,
    enableRemoteModule: boolean,
    preload: string,
  },
  icon?: string,
};

export const createMainWindow = (locale: string) => {
  const windowOptions: WindowOptionsType = {
    show: false,
    width: 1280,
    height: 720 - 23,
    webPreferences: {
      nodeIntegration: isTest,
      webviewTag: false,
      enableRemoteModule: isTest,
      preload: path.join(__dirname, './preload.js'),
      additionalArguments: isBlankScreenFixActive ? ['--safe-mode'] : [],
    },
  };

  if (isLinux) {
    windowOptions.icon = path.join(launcherConfig.statePath, 'icon.png');
  }

  // Construct new BrowserWindow
  const window = new BrowserWindow(windowOptions);

  window.setPosition(0, 0);
  window.setSize(1280, 720 - 73);

  rendererErrorHandler.setup(window, createMainWindow);

  const { minWindowsWidth, minWindowsHeight } = getContentMinimumSize(window);
  window.setMinimumSize(minWindowsWidth, minWindowsHeight);

  // Initialize our ipc api methods that can be called by the render processes
  ipcApi({ window });

  // Provide render process with an api to resize the main window
  ipcMain.on('resize-window', (event, { width, height, animate }) => {
    if (event.sender !== window.webContents) return;
    window.setSize(width, height, animate);
  });

  // Provide render process with an api to close the main window
  ipcMain.on('close-window', event => {
    if (event.sender !== window.webContents) return;
    window.close();
  });

  window.loadURL(`file://${__dirname}/../renderer/index.html`);
  window.on('page-title-updated', event => {
    event.preventDefault();
  });
  window.setTitle(getWindowTitle(locale));

  window.webContents.on('context-menu', (e, props) => {
    const contextMenuOptions = [
      { label: 'Copy', accelerator: 'CmdOrCtrl+C', role: 'copy' },
      { label: 'Paste', accelerator: 'CmdOrCtrl+V', role: 'paste' },
    ];

    if (isDev || isTest) {
      const { x, y } = props;
      contextMenuOptions.push({
        label: 'Inspect element',
        click() {
          window.inspectElement(x, y);
        },
      });
    }

    Menu.buildFromTemplate(contextMenuOptions).popup(window);
  });

  window.webContents.on('did-frame-finish-load', () => {
    if (isDev) {
      window.webContents.openDevTools();
      // Focus the main window after dev tools opened
      window.webContents.on('devtools-opened', () => {
        window.focus();
        setImmediate(() => {
          window.focus();
        });
      });
    }
  });

  window.webContents.on('did-finish-load', () => {
    if (isTest) {
      window.showInactive(); // show without focusing the window
    } else {
      window.show(); // show also focuses the window
    }
  });

  window.on('closed', () => {
    app.quit();
  });

  window.webContents.on('did-fail-load', err => {
    rendererErrorHandler.onError('did-fail-load', err);
  });

  window.webContents.on('crashed', err => {
    rendererErrorHandler.onError('crashed', err);
  });

  window.updateTitle = (locale: string) => {
    window.setTitle(getWindowTitle(locale));
  };

  return window;
};
