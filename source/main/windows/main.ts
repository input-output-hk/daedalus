import path from 'path';
import { app, BrowserWindow, ipcMain, Menu, Rectangle } from 'electron';
import { environment } from '../environment';
import ipcApi from '../ipc';
import RendererErrorHandler from '../utils/rendererErrorHandler';
import { getTranslation } from '../utils/getTranslation';
import { getContentMinimumSize } from '../utils/getContentMinimumSize';
import { buildLabel, launcherConfig } from '../config';
import { ledgerStatus } from '../ipc/getHardwareWalletChannel';

const rendererErrorHandler = new RendererErrorHandler();
const { isDev, isTest, isLinux, isBlankScreenFixActive } = environment;
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
  show: boolean;
  width: number;
  height: number;
  webPreferences: {
    nodeIntegration: boolean;
    webviewTag: boolean;
    enableRemoteModule: boolean;
    preload: string;
  };
  icon?: string;
};
export const createMainWindow = (locale: string, windowBounds?: Rectangle) => {
  const windowOptions: WindowOptionsType = {
    show: false,
    width: 1150,
    height: 870,
    ...windowBounds,
    webPreferences: {
      nodeIntegration: isTest,
      webviewTag: false,
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ nodeIntegration: boolean; webviewTag: fals... Remove this comment to see the full error message
      contextIsolation: false,
      // TODO: change to ipc
      enableRemoteModule: isTest,
      preload: path.join(__dirname, './preload.js'),
      additionalArguments: isBlankScreenFixActive ? ['--safe-mode'] : [],
    },
  };

  if (isLinux) {
    windowOptions.icon = path.join(launcherConfig.stateDir, 'icon.png');
  }

  // Construct new BrowserWindow
  const window = new BrowserWindow(windowOptions);
  rendererErrorHandler.setup(window, createMainWindow);
  const { minWindowsWidth, minWindowsHeight } = getContentMinimumSize(window);
  window.setMinimumSize(minWindowsWidth, minWindowsHeight);
  // Initialize our ipc api methods that can be called by the render processes
  ipcApi(window);
  // Provide render process with an api to resize the main window
  ipcMain.on('resize-window', (event, { width, height, animate }) => {
    if (event.sender !== window.webContents) return;
    window.setSize(width, height, animate);
  });
  // Provide render process with an api to close the main window
  ipcMain.on('close-window', (event) => {
    if (event.sender !== window.webContents) return;
    window.close();
  });
  window.loadURL(`file://${__dirname}/../renderer/index.html`);
  window.on('page-title-updated', (event) => {
    event.preventDefault();
  });
  window.setTitle(getWindowTitle(locale));
  window.webContents.on('context-menu', (e, props) => {
    const { canCopy, canPaste } = props.editFlags;
    const contextMenuOptions = [];

    if (canCopy && props.selectionText) {
      contextMenuOptions.push({
        label: 'Copy',
        accelerator: 'CmdOrCtrl+C',
        role: 'copy',
      });
    }

    if (canPaste) {
      contextMenuOptions.push({
        label: 'Paste',
        accelerator: 'CmdOrCtrl+V',
        role: 'paste',
      });
    }

    if (isDev || isTest) {
      const { x, y } = props;
      contextMenuOptions.push({
        label: 'Inspect element',

        click() {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'inspectElement' does not exist on type '... Remove this comment to see the full error message
          window.inspectElement(x, y);
        },
      });
    }

    if (contextMenuOptions.length) {
      // @ts-ignore ts-migrate(2559) FIXME: Type 'BrowserWindow' has no properties in common w... Remove this comment to see the full error message
      Menu.buildFromTemplate(contextMenuOptions).popup(window);
    }
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
    if (isTest || isDev) {
      window.showInactive(); // show without focusing the window
    } else {
      window.show(); // show also focuses the window
    }
  });

  /**
   * We need to set bounds explicitly because passing them to the
   * window constructor above was buggy (height was not correctly applied)
   */
  window.on('ready-to-show', () => {
    if (windowBounds) {
      window.setBounds(windowBounds);
    }
  });
  window.on('closed', (event) => {
    event.preventDefault();

    if (ledgerStatus.listening && !!ledgerStatus.Listener) {
      ledgerStatus.Listener.unsubscribe();
      setTimeout(() => app.quit(), 5000);
    } else {
      app.quit();
    }
  });
  window.webContents.on('did-fail-load', (err) => {
    rendererErrorHandler.onError('did-fail-load', err);
  });
  window.webContents.on('crashed', (err) => {
    rendererErrorHandler.onError('crashed', err);
  });

  // @ts-ignore ts-migrate(2339) FIXME: Property 'updateTitle' does not exist on type 'Bro... Remove this comment to see the full error message
  window.updateTitle = (locale: string) => {
    window.setTitle(getWindowTitle(locale));
  };

  return window;
};
