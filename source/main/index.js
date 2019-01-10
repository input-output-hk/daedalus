// @flow
import os from 'os';
import { app, BrowserWindow, shell } from 'electron';
import { client } from 'electron-connect';
import { includes } from 'lodash';
import { Logger } from './utils/logging';
import { setupLogging } from './utils/setupLogging';
import { createMainWindow } from './windows/main';
import { installChromeExtensions } from './utils/installChromeExtensions';
import { environment } from './environment';
import mainErrorHandler from './utils/mainErrorHandler';
import { launcherConfig } from './config';
import { setupCardano } from './cardano/setup';
import { CardanoNode } from './cardano/CardanoNode';
import { safeExitWithCode } from './utils/safeExitWithCode';
import { buildAppMenus } from './utils/buildAppMenus';
import { ensureXDGDataIsSet } from './cardano/config';
import { CardanoNodeStates } from '../common/types/cardano-node.types';

// Global references to windows to prevent them from being garbage collected
let mainWindow: BrowserWindow;
let cardanoNode: ?CardanoNode;

const { isDev, isWatchMode, buildLabel } = environment;

const safeExit = async () => {
  if (!cardanoNode) {
    Logger.info('Daedalus:safeExit: exiting Daedalus with code 0.');
    return safeExitWithCode(0);
  }
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

const onAppReady = async () => {
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

  mainWindow.on('close', async (event) => {
    Logger.info('mainWindow received <close> event. Safe exiting Daedalus now.');
    event.preventDefault();
    await safeExit();
  });

  // TODO: Figure out how to get the locale
  // const locale = 'en-US.json';
  const locale = 'ja-JP.json';
  buildAppMenus(mainWindow, cardanoNode, isInSafeMode, locale);

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
};

// Make sure this is the only Daedalus instance running per cluster before doing anything else
const isSingleInstance = app.requestSingleInstanceLock();

if (!isSingleInstance) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
  app.on('ready', onAppReady);
}
