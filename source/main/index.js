import os from 'os';
import { app, globalShortcut, Menu, dialog } from 'electron';
import { Logger } from '../common/logging';
import { client } from 'electron-connect';
import { includes } from 'lodash';
import { setupLogging } from './utils/setupLogging';
import { makeEnvironmentGlobal } from './utils/makeEnvironmentGlobal';
import { createMainWindow } from './windows/main';
import { winLinuxMenu } from './menus/win-linux';
import { osxMenu } from './menus/osx';
import { installChromeExtensions } from './utils/installChromeExtensions';
import environment from '../common/environment';
import { OPEN_ABOUT_DIALOG_CHANNEL } from '../common/ipc-api/open-about-dialog';
import { GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL } from '../common/ipc-api/go-to-ada-redemption-screen';
import mainErrorHandler from './utils/mainErrorHandler';
import { setupCardano } from './cardano/setup';
import { CardanoNode } from './cardano/CardanoNode';
import { flushLogsAndExitWithCode } from './utils/flushLogsAndExitWithCode';

const { LAUNCHER_CONFIG } = process.env;

setupLogging();
mainErrorHandler();

Logger.info(`========== Daedalus is starting at ${new Date()} ==========`);

Logger.debug(`!!! Daedalus is running on ${os.platform()} version ${os.release()}
            with CPU: ${JSON.stringify(os.cpus(), null, 2)} with
            ${JSON.stringify(os.totalmem(), null, 2)} total RAM !!!`);

// Global references to windows to prevent them from being garbage collected
let mainWindow;
let cardanoNode: ?CardanoNode;

const openAbout = () => {
  if (mainWindow) mainWindow.webContents.send(OPEN_ABOUT_DIALOG_CHANNEL);
};

const goToAdaRedemption = () => {
  if (mainWindow) mainWindow.webContents.send(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL);
};

const restartInSafeMode = async () => {
  Logger.info('restarting in SafeMode …');
  if (cardanoNode) await cardanoNode.stop();
  Logger.info('Exiting Daedalus with code 21.');
  flushLogsAndExitWithCode(21);
};

const restartWithoutSafeMode = async () => {
  Logger.info('restarting without SafeMode …');
  if (cardanoNode) await cardanoNode.stop();
  Logger.info('Exiting Daedalus with code 22.');
  flushLogsAndExitWithCode(22);
};

const menuActions = {
  openAbout,
  goToAdaRedemption,
  restartInSafeMode,
  restartWithoutSafeMode,
};

app.on('ready', async () => {
  const isProd = process.env.NODE_ENV === 'production';
  const isStartedByLauncher = !!process.env.LAUNCHER_CONFIG;
  if (isProd && !isStartedByLauncher) {
    const isWindows = process.platform === 'win32';
    const dialogTitle = 'Daedalus improperly started!';
    const dialogMessage = isWindows ?
      'Please start Daedalus using the icon in the Windows start menu or using Daedalus icon on your desktop.' :
      'Daedalus was launched without needed configuration. Please start Daedalus using the shortcut provided by the installer.';
    dialog.showErrorBox(dialogTitle, dialogMessage);
    app.quit();
  }

  makeEnvironmentGlobal(process.env);
  await installChromeExtensions(environment.isDev());

  // Detect safe mode
  const isInSafeMode = includes(process.argv.slice(1), '--safe-mode');

  mainWindow = createMainWindow(isInSafeMode);

  cardanoNode = setupCardano(LAUNCHER_CONFIG, mainWindow);

  if (environment.isDev()) {
    // Connect to electron-connect server which restarts / reloads windows on file changes
    client.create(mainWindow);
  }

  // Build app menus
  let menu;
  if (process.platform === 'darwin') {
    menu = Menu.buildFromTemplate(osxMenu(app, mainWindow, menuActions, isInSafeMode));
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(winLinuxMenu(app, mainWindow, menuActions, isInSafeMode));
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
