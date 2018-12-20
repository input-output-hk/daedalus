// @flow
import os from 'os';
import { app, BrowserWindow, globalShortcut, Menu, dialog, shell } from 'electron';
import { client } from 'electron-connect';
import { includes } from 'lodash';
import { Logger } from './utils/logging';
import { setupLogging } from './utils/setupLogging';
import { createMainWindow } from './windows/main';
import { winLinuxMenu } from './menus/win-linux';
import { osxMenu } from './menus/osx';
import { installChromeExtensions } from './utils/installChromeExtensions';
import { environment } from './environment';
import {
  OPEN_ABOUT_DIALOG_CHANNEL,
  GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL,
  GO_TO_NETWORK_STATUS_SCREEN_CHANNEL
} from '../common/ipc/api';
import mainErrorHandler from './utils/mainErrorHandler';
import { launcherConfig } from './config';
import { setupCardano } from './cardano/setup';
import { CardanoNode } from './cardano/CardanoNode';
import { safeExitWithCode } from './utils/safeExitWithCode';
import { ensureXDGDataIsSet } from './cardano/config';
import { acquireDaedalusInstanceLock } from './utils/lockFiles';
import { CardanoNodeStates } from '../common/types/cardano-node.types';

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
  Logger.info('Restarting in SafeMode...');
  if (cardanoNode) await cardanoNode.stop();
  Logger.info('Exiting Daedalus with code 21.');
  safeExitWithCode(21);
};

const restartWithoutSafeMode = async () => {
  Logger.info('Restarting without SafeMode...');
  if (cardanoNode) await cardanoNode.stop();
  Logger.info('Exiting Daedalus with code 22.');
  safeExitWithCode(22);
};

const { isDev, isMacOS, isWatchMode, buildLabel } = environment;
const menuActions = {
  openAbout,
  goToAdaRedemption,
  goToNetworkStatus,
  restartInSafeMode,
  restartWithoutSafeMode,
};

const safeExit = async () => {
  if (cardanoNode.state === CardanoNodeStates.STOPPING) return;
  try {
    Logger.info(`Daedalus:safeExit: stopping cardano-node with PID ${cardanoNode.pid || 'null'}`);
    await cardanoNode.stop();
    Logger.info('Daedalus:safeExit: exiting Daedalus with code 0.');
    safeExitWithCode(0);
  } catch (stopError) {
    Logger.info(`Daedalus:safeExit: cardano-node did not exit correctly: ${stopError}`);
    safeExitWithCode(0);
  }
};

app.on('ready', async () => {
  // Make sure this is the only Daedalus instance running per cluster before doing anything else
  try {
    await acquireDaedalusInstanceLock();
  } catch (e) {
    const dialogTitle = 'Daedalus is unable to start!';
    const dialogMessage = 'Another Daedalus instance is already running.';
    dialog.showErrorBox(dialogTitle, dialogMessage);
    app.exit(1);
  }

  setupLogging();
  mainErrorHandler();

  Logger.info(`========== Daedalus is starting at ${new Date().toString()} ==========`);

  Logger.debug(`!!! ${buildLabel} is running on ${os.platform()} version ${os.release()}
            with CPU: ${JSON.stringify(os.cpus(), null, 2)} with
            ${JSON.stringify(os.totalmem(), null, 2)} total RAM !!!`);

  ensureXDGDataIsSet();
  await installChromeExtensions(isDev);

  // Detect safe mode
  const isInSafeMode = includes(process.argv.slice(1), '--safe-mode');

  mainWindow = createMainWindow(isInSafeMode);
  cardanoNode = setupCardano(launcherConfig, mainWindow);

  if (isWatchMode) {
    // Connect to electron-connect server which restarts / reloads windows on file changes
    client.create(mainWindow);
  }

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

  mainWindow.on('close', async (event) => {
    Logger.info('mainWindow received <close> event. Safe exiting Daedalus now.');
    event.preventDefault();
    await safeExit();
  });

  // Security feature: Prevent creation of new browser windows
  // https://github.com/electron/electron/blob/master/docs/tutorial/security.md#14-disable-or-limit-creation-of-new-windows
  app.on('web-contents-created', (_, contents) => {
    contents.on('new-window', (event, url) => {
      // Prevent creation of new BrowserWindows via links / window.open
      event.preventDefault();
      Logger.info(`Prevented creation of new browser window with url ${url}`);
      // Open these links with the default browser
      shell.openExternal(url);
    });
  });

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    Logger.info('app received <before-quit> event. Safe exiting Daedalus now.');
    event.preventDefault(); // prevent Daedalus from quitting immediately
    await safeExit();
  });
});
