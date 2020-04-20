// @flow
import os from 'os';
import path from 'path';
import { app, BrowserWindow, shell } from 'electron';
import { client } from 'electron-connect';
import { logger } from './utils/logging';
import {
  setupLogging,
  logSystemInfo,
  logStateSnapshot,
  generateWalletMigrationReport,
} from './utils/setupLogging';
import { handleDiskSpace } from './utils/handleDiskSpace';
import { createMainWindow } from './windows/main';
import { installChromeExtensions } from './utils/installChromeExtensions';
import { environment } from './environment';
import mainErrorHandler from './utils/mainErrorHandler';
import {
  launcherConfig,
  pubLogsFolderPath,
  stateDirectoryPath,
} from './config';
import { setupCardanoNode } from './cardano/setup';
import { CardanoNode } from './cardano/CardanoNode';
import { safeExitWithCode } from './utils/safeExitWithCode';
import { buildAppMenus } from './utils/buildAppMenus';
import { getLocale } from './utils/getLocale';
import { detectSystemLocale } from './utils/detectSystemLocale';
import { ensureXDGDataIsSet } from './cardano/config';
import { rebuildApplicationMenu } from './ipc/rebuild-application-menu';
import { detectSystemLocaleChannel } from './ipc/detect-system-locale';
import { getStateDirectoryPathChannel } from './ipc/getStateDirectoryPathChannel';
import { CardanoNodeStates } from '../common/types/cardano-node.types';
import type { CheckDiskSpaceResponse } from '../common/types/no-disk-space.types';
import { logUsedVersion } from './utils/logUsedVersion';
import { setStateSnapshotLogChannel } from './ipc/set-log-state-snapshot';
import { generateWalletMigrationReportChannel } from './ipc/generateWalletMigrationReportChannel';

/* eslint-disable consistent-return */

// Global references to windows to prevent them from being garbage collected
let mainWindow: BrowserWindow;
let cardanoNode: ?CardanoNode;

const {
  isDev,
  isWatchMode,
  isBlankScreenFixActive,
  network,
  os: osName,
  version: daedalusVersion,
  buildNumber: cardanoVersion,
} = environment;

if (isBlankScreenFixActive) {
  // Run "location.assign('chrome://gpu')" in JavaScript console to see if the flag is active
  app.disableHardwareAcceleration();
}

const safeExit = async () => {
  if (!cardanoNode || cardanoNode.state === CardanoNodeStates.STOPPED) {
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', { code: 0 });
    return safeExitWithCode(0);
  }
  if (cardanoNode.state === CardanoNodeStates.STOPPING) {
    logger.info('Daedalus:safeExit: waiting for cardano-node to stop...');
    cardanoNode.exitOnStop();
    return;
  }
  try {
    const pid = cardanoNode.pid || 'null';
    logger.info(`Daedalus:safeExit: stopping cardano-node with PID: ${pid}`, {
      pid,
    });
    await cardanoNode.stop();
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', { code: 0 });
    safeExitWithCode(0);
  } catch (error) {
    logger.error('Daedalus:safeExit: cardano-node did not exit correctly', {
      error,
    });
    safeExitWithCode(0);
  }
};

const onAppReady = async () => {
  setupLogging();
  logUsedVersion(
    environment.version,
    path.join(pubLogsFolderPath, 'Daedalus-versions.json')
  );

  const cpu = os.cpus();
  const platformVersion = os.release();
  const ram = JSON.stringify(os.totalmem(), null, 2);
  const startTime = new Date().toISOString();
  // first checks for japanese locale, otherwise returns english
  const systemLocale = detectSystemLocale();

  const systemInfo = logSystemInfo({
    cardanoVersion,
    cpu,
    daedalusVersion,
    isBlankScreenFixActive,
    network,
    osName,
    platformVersion,
    ram,
    startTime,
  });

  logger.info(`Daedalus is starting at ${startTime}`, { startTime });

  logger.info('Updating System-info.json file', { ...systemInfo.data });

  // We need DAEDALUS_INSTALL_DIRECTORY in PATH
  // in order for the cardano-launcher to find wallet and node bins
  process.env.PATH = [
    process.env.PATH,
    process.env.DAEDALUS_INSTALL_DIRECTORY,
  ].join(path.delimiter);

  ensureXDGDataIsSet();
  await installChromeExtensions(isDev);

  // Detect locale
  let locale = getLocale(network);

  mainWindow = createMainWindow(locale);

  const onCheckDiskSpace = ({
    isNotEnoughDiskSpace,
  }: CheckDiskSpaceResponse) => {
    if (cardanoNode) {
      if (isNotEnoughDiskSpace) {
        if (
          cardanoNode.state !== CardanoNodeStates.STOPPING &&
          cardanoNode.state !== CardanoNodeStates.STOPPED
        ) {
          try {
            cardanoNode.stop();
          } catch (e) {} // eslint-disable-line
        }
      } else if (
        cardanoNode.state !== CardanoNodeStates.STARTING &&
        cardanoNode.state !== CardanoNodeStates.RUNNING
      ) {
        cardanoNode.restart();
      }
    }
  };
  const handleCheckDiskSpace = handleDiskSpace(mainWindow, onCheckDiskSpace);
  const onMainError = (error: string) => {
    if (error.indexOf('ENOSPC') > -1) {
      handleCheckDiskSpace();
      return false;
    }
  };
  mainErrorHandler(onMainError);
  await handleCheckDiskSpace();

  cardanoNode = setupCardanoNode(launcherConfig, mainWindow, locale);

  if (isWatchMode) {
    // Connect to electron-connect server which restarts / reloads windows on file changes
    client.create(mainWindow);
  }

  detectSystemLocaleChannel.onRequest(() => Promise.resolve(systemLocale));

  setStateSnapshotLogChannel.onReceive(data => {
    return Promise.resolve(logStateSnapshot(data));
  });

  generateWalletMigrationReportChannel.onReceive(data => {
    return Promise.resolve(generateWalletMigrationReport(data));
  });

  getStateDirectoryPathChannel.onRequest(() =>
    Promise.resolve(stateDirectoryPath)
  );

  mainWindow.on('close', async event => {
    logger.info(
      'mainWindow received <close> event. Safe exiting Daedalus now.'
    );
    event.preventDefault();
    await safeExit();
  });

  buildAppMenus(mainWindow, cardanoNode, locale, { isUpdateAvailable: false });

  await rebuildApplicationMenu.onReceive(
    data =>
      new Promise(resolve => {
        locale = getLocale(network);
        buildAppMenus(mainWindow, cardanoNode, locale, {
          isUpdateAvailable: data.isUpdateAvailable,
        });
        mainWindow.updateTitle(locale);
        resolve();
      })
  );

  // Security feature: Prevent creation of new browser windows
  // https://github.com/electron/electron/blob/master/docs/tutorial/security.md#14-disable-or-limit-creation-of-new-windows
  app.on('web-contents-created', (_, contents) => {
    contents.on('new-window', (event, url) => {
      // Prevent creation of new BrowserWindows via links / window.open
      event.preventDefault();
      logger.info('Prevented creation of new browser window', { url });
      // Open these links with the default browser
      shell.openExternal(url);
    });
  });

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async event => {
    logger.info('app received <before-quit> event. Safe exiting Daedalus now.');
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
