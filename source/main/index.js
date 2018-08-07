import os from 'os';
import { app, globalShortcut, Menu } from 'electron';
import log from 'electron-log';
import { client } from 'electron-connect';
import { setupLogging } from './utils/setupLogging';
import { setupTls } from './utils/setupTls';
import { makeEnvironmentGlobal } from './utils/makeEnvironmentGlobal';
import { createMainWindow } from './windows/main';
import { winLinuxMenu } from './menus/win-linux';
import { osxMenu } from './menus/osx';
import { installChromeExtensions } from './utils/installChromeExtensions';
import environment from '../common/environment';
import { OPEN_ABOUT_DIALOG_CHANNEL } from '../common/ipc-api/open-about-dialog';
import { GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL } from '../common/ipc-api/go-to-ada-redemption-screen';
import mainErrorHandler from './utils/mainErrorHandler';

setupLogging();
mainErrorHandler();

log.info(`========== Daedalus is starting at ${new Date()} ==========`);

log.info(`!!! Daedalus is running on ${os.platform()} version ${os.release()}
            with CPU: ${JSON.stringify(os.cpus(), null, 2)} with
            ${JSON.stringify(os.totalmem(), null, 2)} total RAM !!!`);

// Global references to windows to prevent them from being garbage collected
let mainWindow;

const openAbout = () => {
  if (mainWindow) mainWindow.webContents.send(OPEN_ABOUT_DIALOG_CHANNEL);
};

const goToAdaRedemption = () => {
  if (mainWindow) mainWindow.webContents.send(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL);
};

const restartInSafeMode = () => {
  app.relaunch({ args: process.argv.slice(1).concat(['--relaunch', '--disable-gpu']) });
  app.exit(0);
};

const menuActions = {
  openAbout,
  goToAdaRedemption,
  restartInSafeMode
};

app.on('ready', async () => {
  setupTls();
  makeEnvironmentGlobal(process.env);
  await installChromeExtensions(environment.isDev());
  mainWindow = createMainWindow();

  if (environment.isDev()) {
    // Connect to electron-connect server which restarts / reloads windows on file changes
    client.create(mainWindow);
  }

  // Build app menus
  let menu;
  if (process.platform === 'darwin') {
    menu = Menu.buildFromTemplate(osxMenu(app, mainWindow, menuActions));
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(winLinuxMenu(app, mainWindow, menuActions));
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
