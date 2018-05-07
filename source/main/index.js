import os from 'os';
import { app, globalShortcut, Menu } from 'electron';
import log from 'electron-log';
import { client } from 'electron-connect';
import { setupLogging } from './utils/setupLogging';
import { setupTls } from './utils/setupTls';
import { setupCardano } from './utils/ada.js';
import { makeEnvironmentGlobal } from './utils/makeEnvironmentGlobal';
import { createMainWindow } from './windows/main';
import { createAboutWindow } from './windows/about';
import { winLinuxMenu } from './menus/win-linux';
import { osxMenu } from './menus/osx';
import { installChromeExtensions } from './utils/installChromeExtensions';
import environment from '../common/environment';

setupLogging();

log.info(`========== Daedalus is starting at ${new Date()} ==========`);

log.info(`!!! Daedalus is running on ${os.platform()} version ${os.release()}
            with CPU: ${JSON.stringify(os.cpus(), null, 2)} with
            ${JSON.stringify(os.totalmem(), null, 2)} total RAM !!!`);

// Global references to windows to prevent them from being garbage collected
let mainWindow;
let aboutWindow;

const openAbout = () => {
  if (aboutWindow) aboutWindow.show(); // show also focuses the window
};

app.on('ready', async () => {
  setupTls();
  setupCardano();
  makeEnvironmentGlobal(process.env);
  await installChromeExtensions(environment.isDev());
  aboutWindow = createAboutWindow();
  mainWindow = createMainWindow();

  if (environment.isDev()) {
    // Connect to electron-connect server which restarts / reloads windows on file changes
    client.create(aboutWindow);
    client.create(mainWindow);
  }

  // Build app menus
  let menu;
  if (process.platform === 'darwin') {
    menu = Menu.buildFromTemplate(osxMenu(app, mainWindow, openAbout));
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(winLinuxMenu(app, mainWindow, openAbout));
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

app.on('window-all-closed', () => {
  app.quit();
});
