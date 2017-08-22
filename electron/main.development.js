import { app, BrowserWindow, Menu, shell, ipcMain, dialog, crashReporter, globalShortcut } from 'electron';
import os from 'os';
import path from 'path';
import Log from 'electron-log';
import { Tail } from 'tail';
import osxMenu from './menus/osx';
import winLinuxMenu from './menus/win-linux';
import ipcApi from './ipc-api';
import getLogsFolderPath from './lib/getLogsFolderPath';
import { daedalusLogger, cardanoNodeLogger } from './lib/remoteLog';
import ClientApi from 'daedalus-client-api';
import { readCA, notify } from './tls-workaround';

const APP_NAME = 'Daedalus';
// Configure default logger levels for console and file outputs
const appLogFolderPath = getLogsFolderPath(process.platform, process.env, APP_NAME);
const logFilePath = path.join(appLogFolderPath, APP_NAME + '.log');
Log.transports.console.level = 'warn';
Log.transports.file.level = 'debug';
Log.transports.file.file = logFilePath;

try {
  let sendLogsToRemoteServer;
  let isLogTailingActive = false;
  ipcMain.on('send-logs-choice', (event, sendLogs) => {
    // Save user's send-logs choice
    sendLogsToRemoteServer = sendLogs;

    // Start log tailing if it's not running
    if (!isLogTailingActive) {
      // Tail Daedalus log and send it to remote logging server
      const daedalusLogTail = new Tail(logFilePath);
      daedalusLogTail.on('line', (line) => {
        if (sendLogsToRemoteServer) daedalusLogger.info(line);
      });

      // Tail Cardano node log and send it to remote logging server
      const cardanoNodeLogFilePath = path.join(appLogFolderPath, 'cardano-node.log');
      const cardanoNodeLogTail = new Tail(cardanoNodeLogFilePath);
      cardanoNodeLogTail.on('line', (line) => {
        if (sendLogsToRemoteServer) cardanoNodeLogger.info(line);
      });

      // Tailing is now active (no need re-init it on next send-logs-choice event)
      isLogTailingActive = true;
    }
  });
} catch (error) {
  Log.error('Error setting up log tailing and logging to remote server', error);
}

// Configure & start crash reporter
app.setPath('temp', appLogFolderPath);

// TODO: Update when endpoint is ready (crash reports are only saved locally for now)
crashReporter.start({
  companyName: 'IOHK',
  productName: APP_NAME,
  submitURL: '',
  uploadToServer: false
});

Log.info(`========== Daedalus is starting at ${new Date()} ==========`);
Log.info(`!!! Daedalus is running on ${os.platform()} version ${os.release()}
with CPU: ${JSON.stringify(os.cpus(), null, 2)} with ${JSON.stringify(os.totalmem(), null, 2)} total RAM !!!`);

let menu;
let mainWindow = null;
const isDev = process.env.NODE_ENV === 'development';
const isProd = process.env.NODE_ENV === 'production';
const isTest = process.env.NODE_ENV === 'test';
const daedalusVersion = process.env.DAEDALUS_VERSION || 'dev';

if (isDev) {
  require('electron-debug')(); // eslint-disable-line global-require
}

app.on('window-all-closed', () => app.quit());

const installExtensions = async () => {
  if (isDev) {
    const installer = require('electron-devtools-installer'); // eslint-disable-line global-require

    const extensions = [
      'REACT_DEVELOPER_TOOLS',
    ];
    const forceDownload = !!process.env.UPGRADE_EXTENSIONS;
    for (const name of extensions) {
      try {
        await installer.default(installer[name], forceDownload);
      } catch (e) {
      } // eslint-disable-line
    }
  }
};

app.on('ready', async () => {
  await installExtensions();

  try {

    const ca = readCA(path.join(__dirname, '../tls/ca.crt'));

    const tlsConfig = ClientApi.tlsInit(ca);
    let messageCallback, errorCallback = null;

    notify(
      ca,
      function handleNotifyMessage(...args) {
        if (messageCallback) {
          try {
            messageCallback(...args);
          } catch (e) {
            // The callback might have been released on page refresh etc.
            messageCallback = null;
          }
        }
      },
      function handleNotifyError(...args) {
        if (errorCallback) {
          try {
            errorCallback(...args);
          } catch (e) {
            // The callback might have been released on page refresh etc.
            errorCallback = null;
          }
        }
      }
    );
    Object.assign(global, {
      tlsConfig,
      registerNotifyCallback: (onMessage, onError) => {
        messageCallback = onMessage;
        errorCallback = onError;
      },
    });
  } catch(error) {
    Log.info("error:", error);
  }

  mainWindow = new BrowserWindow({
    show: false,
    width: 1150,
    height: 870
  });

  mainWindow.setMinimumSize(900, 600);
  mainWindow.setMaximumSize(1500, 2500);

  // Initialize our ipc api methods that can be called by the render processes
  ipcApi({ mainWindow });

  mainWindow.loadURL(`file://${__dirname}/../app/index.html` + (isTest ? '?test=true' : ''));
  mainWindow.on('page-title-updated', event => {
   event.preventDefault()
  });
  mainWindow.setTitle(`Daedalus (${daedalusVersion})`);

  mainWindow.webContents.on('did-finish-load', () => {
    mainWindow.show();
    mainWindow.focus();
  });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });

  if (isDev) mainWindow.openDevTools();

  mainWindow.webContents.on('context-menu', (e, props) => {
    const contextMenuOptions = [
      { label: 'Copy', accelerator: 'CmdOrCtrl+C', selector: 'copy:' },
      { label: 'Paste', accelerator: 'CmdOrCtrl+V', selector: 'paste:' },
    ];

    if (isDev || isTest) {
      const { x, y } = props;
      contextMenuOptions.push({
        label: 'Inspect element',
        click() {
          mainWindow.inspectElement(x, y);
        }
      });
    }

    Menu.buildFromTemplate(contextMenuOptions).popup(mainWindow);
  });

  if (process.platform === 'darwin') {
    menu = Menu.buildFromTemplate(osxMenu(app, mainWindow));
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(winLinuxMenu(mainWindow));
    mainWindow.setMenu(menu);
  }

  // Hide application window on Cmd+H hotkey (OSX only!)
  if (process.platform === 'darwin') {
    app.on('activate', () => {
      if (!mainWindow.isVisible()) app.show();
    });

    mainWindow.on('focus', () => {
      globalShortcut.register('CommandOrControl+H', app.hide);
    });

    mainWindow.on('blur', () => {
      globalShortcut.unregister('CommandOrControl+H');
    });
  }

});
