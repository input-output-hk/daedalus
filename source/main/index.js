// @flow
import os from 'os';
import { app, BrowserWindow, globalShortcut, Menu, dialog } from 'electron';
import { client } from 'electron-connect';
import { includes } from 'lodash';
import { Logger } from '../common/logging';
import { setupLogging } from './utils/setupLogging';
import { createMainWindow } from './windows/main';
import { winLinuxMenu } from './menus/win-linux';
import { osxMenu } from './menus/osx';
import { installChromeExtensions } from './utils/installChromeExtensions';
import { environment } from '../common/environment';
import { OPEN_ABOUT_DIALOG_CHANNEL } from '../common/ipc/open-about-dialog';
import { GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL } from '../common/ipc/go-to-ada-redemption-screen';
import { GO_TO_NETWORK_STATUS_SCREEN_CHANNEL } from '../common/ipc/go-to-network-status-screen';
import mainErrorHandler from './utils/mainErrorHandler';
import { launcherConfig } from './config';
import { setupCardano } from './cardano/setup';
import { CardanoNode } from './cardano/CardanoNode';
import { safeExitWithCode } from './utils/safeExitWithCode';
import { ensureXDGDataIsSet } from './cardano/config';
import { acquireDaedalusInstanceLock } from './utils/lockFiles';
import { CardanoNodeStates } from '../common/types/cardanoNode.types';

// Global references to windows to prevent them from being garbage collected
let mainWindow: BrowserWindow;
let cardanoNode: CardanoNode;

const openAbout = () => {
  if (mainWindow) mainWindow.webContents.send(OPEN_ABOUT_DIALOG_CHANNEL);
};

const goToAdaRedemption = () => {
  if (mainWindow) mainWindow.webContents.send(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL);
};

const goToNetworkStatus = () => {
  if (mainWindow) mainWindow.webContents.send(GO_TO_NETWORK_STATUS_SCREEN_CHANNEL);
};

const restartInSafeMode = async () => {
  Logger.info('restarting in SafeMode …');
  if (cardanoNode) await cardanoNode.stop();
  Logger.info('Exiting Daedalus with code 21.');
  safeExitWithCode(21);
};

const restartWithoutSafeMode = async () => {
  Logger.info('restarting without SafeMode …');
  if (cardanoNode) await cardanoNode.stop();
  Logger.info('Exiting Daedalus with code 22.');
  safeExitWithCode(22);
};

const { isDev, isProduction, isMacOS, isWindows } = environment;
const menuActions = {
  openAbout,
  goToAdaRedemption,
  goToNetworkStatus,
  restartInSafeMode,
  restartWithoutSafeMode,
};

app.on('ready', async () => {
  // Make sure this is the only Daedalus instance running per cluster before doing anything else
  await acquireDaedalusInstanceLock();
  setupLogging();
  mainErrorHandler();

  Logger.info(`========== Daedalus is starting at ${new Date().toString()} ==========`);

  Logger.debug(`!!! Daedalus is running on ${os.platform()} version ${os.release()}
            with CPU: ${JSON.stringify(os.cpus(), null, 2)} with
            ${JSON.stringify(os.totalmem(), null, 2)} total RAM !!!`);

  const isStartedByLauncher = !!process.env.LAUNCHER_CONFIG;
  if (isProduction && !isStartedByLauncher) {
    const dialogTitle = 'Daedalus improperly started!';
    const dialogMessage = isWindows ?
      'Please start Daedalus using the icon in the Windows start menu or using Daedalus icon on your desktop.' :
      'Daedalus was launched without needed configuration. Please start Daedalus using the shortcut provided by the installer.';
    dialog.showErrorBox(dialogTitle, dialogMessage);
    app.quit();
  }

  ensureXDGDataIsSet();
  await installChromeExtensions(isDev);

  // Detect safe mode
  const isInSafeMode = includes(process.argv.slice(1), '--safe-mode');

  mainWindow = createMainWindow(isInSafeMode);
  cardanoNode = setupCardano(launcherConfig, mainWindow);

  // If NODE_ENV is running in development mode,
  // Connect to electron-connect server which restarts / reloads windows on file changes
  if (isDev) { client.create(mainWindow); }

  // Build app menus
  let menu;
  if (isMacOS) {
    menu = Menu.buildFromTemplate(osxMenu(app, mainWindow, menuActions, isInSafeMode));
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(winLinuxMenu(app, mainWindow, menuActions, isInSafeMode));
    mainWindow.setMenu(menu);
  }

  // Hide application window on Cmd+H hotkey (OSX only!)
  if (isMacOS) {
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

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    event.preventDefault(); // prevent Daedalus from quitting immediately
    if (cardanoNode.state === CardanoNodeStates.STOPPING) return;
    try {
      Logger.info(`Daedalus:before-quit: stopping cardano-node with PID ${cardanoNode.pid || 'null'}`);
      await cardanoNode.stop();
      Logger.info('Daedalus:before-quit: exiting Daedalus with code 0.');
      safeExitWithCode(0);
    } catch (stopError) {
      Logger.info(`Daedalus:before-quit: cardano-node did not exit correctly: ${stopError}`);
      safeExitWithCode(0);
    }
  });
});
