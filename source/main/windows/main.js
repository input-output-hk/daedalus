'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.createMainWindow = void 0;
const path_1 = __importDefault(require('path'));
const electron_1 = require('electron');
const environment_1 = require('../environment');
const ipc_1 = __importDefault(require('../ipc'));
const rendererErrorHandler_1 = __importDefault(
  require('../utils/rendererErrorHandler')
);
const getTranslation_1 = require('../utils/getTranslation');
const getContentMinimumSize_1 = require('../utils/getContentMinimumSize');
const config_1 = require('../config');
const getHardwareWalletChannel_1 = require('../ipc/getHardwareWalletChannel');
const rtsFlagsSettings_1 = require('../utils/rtsFlagsSettings');
const rendererErrorHandler = new rendererErrorHandler_1.default();
const {
  isDev,
  isTest,
  isLinux,
  isBlankScreenFixActive,
  network,
} = environment_1.environment;
const rtsFlags = (0, rtsFlagsSettings_1.getRtsFlagsSettings)(network);
const id = 'window';
const getWindowTitle = (locale) => {
  const translations = require(`../locales/${locale}`);
  const translation = (0, getTranslation_1.getTranslation)(translations, id);
  let title = config_1.buildLabel;
  if (isBlankScreenFixActive)
    title += ` ${translation('title.blankScreenFix')}`;
  if (!!rtsFlags && rtsFlags?.length > 0)
    title += ` ${translation('title.usingRtsFlags')}`;
  return title;
};
const createMainWindow = (locale, getSavedWindowBounds) => {
  const windowOptions = {
    show: false,
    width: 1150,
    height: 870,
    ...getSavedWindowBounds(),
    webPreferences: {
      nodeIntegration: true,
      webviewTag: false,
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ nodeIntegration: boolean; webviewTag: fals... Remove this comment to see the full error message
      contextIsolation: false,
      // TODO: change to ipc
      enableRemoteModule: isTest,
      preload: path_1.default.join(__dirname, './preload.js'),
      additionalArguments: isBlankScreenFixActive ? ['--safe-mode'] : [],
    },
  };
  if (isLinux) {
    windowOptions.icon = path_1.default.join(
      config_1.launcherConfig.stateDir,
      'icon.png'
    );
  }
  // Construct new BrowserWindow
  const window = new electron_1.BrowserWindow(windowOptions);
  rendererErrorHandler.setup(window, exports.createMainWindow);
  const { minWindowsWidth, minWindowsHeight } = (0,
  getContentMinimumSize_1.getContentMinimumSize)(window);
  window.setMinimumSize(minWindowsWidth, minWindowsHeight);
  // Initialize our ipc api methods that can be called by the render processes
  (0, ipc_1.default)(window);
  // Provide render process with an api to resize the main window
  electron_1.ipcMain.on(
    'resize-window',
    (event, { width, height, animate }) => {
      if (event.sender !== window.webContents) return;
      window.setSize(width, height, animate);
    }
  );
  // Provide render process with an api to close the main window
  electron_1.ipcMain.on('close-window', (event) => {
    if (event.sender !== window.webContents) return;
    window.close();
  });
  if (isDev) {
    window.loadURL(`http://127.0.0.1:8080`);
  } else {
    window.loadURL(`file://${__dirname}/../renderer/index.html`);
  }
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
      electron_1.Menu.buildFromTemplate(contextMenuOptions).popup(window);
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
    const savedWindowBounds = getSavedWindowBounds();
    if (savedWindowBounds) {
      window.setBounds(savedWindowBounds);
    }
  });
  window.on('closed', (event) => {
    event.preventDefault();
    if (
      getHardwareWalletChannel_1.ledgerStatus.listening &&
      !!getHardwareWalletChannel_1.ledgerStatus.Listener
    ) {
      getHardwareWalletChannel_1.ledgerStatus.Listener.unsubscribe();
      setTimeout(() => electron_1.app.quit(), 5000);
    } else {
      electron_1.app.quit();
    }
  });
  window.webContents.on('did-fail-load', (err) => {
    rendererErrorHandler.onError('did-fail-load', err);
  });
  window.webContents.on('crashed', (err) => {
    rendererErrorHandler.onError('crashed', err);
  });
  // @ts-ignore ts-migrate(2339) FIXME: Property 'updateTitle' does not exist on type 'Bro... Remove this comment to see the full error message
  window.updateTitle = (locale) => {
    window.setTitle(getWindowTitle(locale));
  };
  return window;
};
exports.createMainWindow = createMainWindow;
//# sourceMappingURL=main.js.map
