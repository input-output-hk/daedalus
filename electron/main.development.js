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

  mainWindow = new BrowserWindow({
    show: false,
    width: 1150,
    height: 870
  });

  mainWindow.setMinimumSize(900, 600);
  mainWindow.setMaximumSize(1500, 2500);

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
