import { app, BrowserWindow, Menu, shell, ipcMain, dialog } from 'electron';
import osxMenu from './menus/osx';
import fs from 'fs';
import winLinuxMenu from './menus/win-linux';
import ipcApi from './ipc-api';

let menu;
let mainWindow = null;
const isDev = process.env.NODE_ENV === 'development';
const isProd = process.env.NODE_ENV === 'production';
const daedalusVersion = process.env.DAEDALUS_VERSION || 'dev';

if (isDev) {
  require('electron-debug')(); // eslint-disable-line global-require
}

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

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

  if (isProd) {
    const DATA = process.env.APPDATA || (process.platform == 'darwin' ? process.env.HOME + 'Library/Preferences' : '~/.config');

    const logfile = fs.openSync(DATA + '\\Daedalus\\Logs\\cardano-node.log', 'a');

    const cardanoFlags = [
      '--listen', '0.0.0.0:12100',
      '--peer', '35.156.182.24:3000/MHdrsP-oPf7UWl0007QuXnLK5RD=',
      '--peer', '54.183.103.204:3000/MHdrsP-oPf7UWl0077QuXnLK5RD=',
      '--peer', '52.53.231.169:3000/MHdrsP-oPf7UWl0127QuXnLK5RD=',
      '--peer', '35.157.41.94:3000/MHdrsP-oPf7UWl0057QuXnLK5RD=',
      '--log-config', 'log-config-prod.yaml',
      '--keyfile', DATA + '\\Daedalus\\Secrets\\secret.key',
      '--logs-prefix', DATA + '\\Daedalus\\Logs',
      '--db-path', DATA + '\\Daedalus\\DB',
      '--wallet-db-path', DATA + '\\Daedalus\\Wallet',
      '--wallet',
    ];

    // TODO: based on platform, different command
    const cardanoNode = require('child_process').spawn('cardano-node.exe', cardanoFlags, {stdio: ['ignore', logfile, logfile]});
    cardanoNode.on('error', error => {
      dialog.showErrorBox('cardano-node exited', error.name + ": " + error.message);
      app.quit()
    });
  }

  mainWindow = new BrowserWindow({
    show: false,
    width: 1150,
    height: 870
  });

  // Initialize our ipc api methods that can be called by the render processes
  ipcApi({ mainWindow });

  mainWindow.loadURL(`file://${__dirname}/../app/index.html`);
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
    if (isProd) {
      cardanoNode.kill('SIGINT');
    }
  });

  if (isDev) mainWindow.openDevTools();

  mainWindow.webContents.on('context-menu', (e, props) => {
    const { x, y } = props;

    Menu.buildFromTemplate([{
      label: 'Inspect element',
      click() {
        mainWindow.inspectElement(x, y);
      }
    }]).popup(mainWindow);
  });

  if (process.platform === 'darwin') {
    menu = Menu.buildFromTemplate(osxMenu(app, mainWindow));
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(winLinuxMenu(mainWindow));
    mainWindow.setMenu(menu);
  }
});
