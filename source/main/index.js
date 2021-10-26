// @flow
import os from 'os';
import path from 'path';
import { app, dialog, BrowserWindow, screen, shell } from 'electron';
import childProcess from 'child_process';
import { client } from 'electron-connect';
import EventEmitter from 'events';
import { requestElectronStore } from './ipc/electronStoreConversation';
import { logger } from './utils/logging';
import {
  setupLogging,
  logSystemInfo,
  logStateSnapshot,
  generateWalletMigrationReport,
} from './utils/setupLogging';
import { handleDiskSpace } from './utils/handleDiskSpace';
// import { handleCustomProtocol } from './utils/handleCustomProtocol';
import { handleCheckBlockReplayProgress } from './utils/handleCheckBlockReplayProgress';
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
import { getStateDirectoryPathChannel } from './ipc/getStateDirectoryPathChannel';
import { getDesktopDirectoryPathChannel } from './ipc/getDesktopDirectoryPathChannel';
import { getSystemLocaleChannel } from './ipc/getSystemLocaleChannel';
import { CardanoNodeStates } from '../common/types/cardano-node.types';
import type { CheckDiskSpaceResponse } from '../common/types/no-disk-space.types';
import type {
  GenerateWalletMigrationReportRendererRequest,
  SetStateSnapshotLogMainResponse,
} from '../common/ipc/api';
import { logUsedVersion } from './utils/logUsedVersion';
import { setStateSnapshotLogChannel } from './ipc/set-log-state-snapshot';
import { generateWalletMigrationReportChannel } from './ipc/generateWalletMigrationReportChannel';
import { enableApplicationMenuNavigationChannel } from './ipc/enableApplicationMenuNavigationChannel';
import { pauseActiveDownloads } from './ipc/downloadManagerChannel';
import {
  restoreSavedWindowBounds,
  saveWindowBoundsOnSizeAndPositionChange,
} from './windows/windowBounds';

/* eslint-disable consistent-return */

// Global references to windows to prevent them from being garbage collected
let mainWindow: BrowserWindow;
let cardanoNode: ?CardanoNode;
let deeplinkingUrl;

const {
  isDev,
  isTest,
  isWatchMode,
  isBlankScreenFixActive,
  isSelfnode,
  network,
  os: osName,
  version: daedalusVersion,
  nodeVersion: cardanoNodeVersion,
  apiVersion: cardanoWalletVersion,
  keepLocalClusterRunning,
} = environment;

if (isBlankScreenFixActive) {
  // Run "console.log(JSON.stringify(daedalus.stores.app.gpuStatus, null, 2))"
  // in DevTools JavaScript console to see if the flag is active
  app.disableHardwareAcceleration();
}

// Increase maximum event listeners to avoid IPC channel stalling
// (1/2) this line increases the limit for the main process
EventEmitter.defaultMaxListeners = 100; // Default: 10

app.allowRendererProcessReuse = true;
const safeExit = async () => {
  pauseActiveDownloads();
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
  logger.info('[Custom-Protocol] APP READY');
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
    cardanoNodeVersion,
    cardanoWalletVersion,
    cpu,
    daedalusVersion,
    isBlankScreenFixActive,
    network,
    osName,
    platformVersion,
    ram,
    startTime,
  });

  // We need DAEDALUS_INSTALL_DIRECTORY in PATH in order for the
  // cardano-launcher to find cardano-wallet and cardano-node executables
  process.env.PATH = [
    process.env.PATH,
    process.env.DAEDALUS_INSTALL_DIRECTORY,
  ].join(path.delimiter);

  logger.info(`Daedalus is starting at ${startTime}`, { startTime });

  logger.info('Updating System-info.json file', { ...systemInfo.data });

  logger.info(`Current working directory is: ${process.cwd()}`, {
    cwd: process.cwd(),
  });

  ensureXDGDataIsSet();
  await installChromeExtensions(isDev);

  // Detect locale
  let locale = getLocale(network);
  mainWindow = createMainWindow(
    locale,
    restoreSavedWindowBounds(screen, requestElectronStore)
  );
  saveWindowBoundsOnSizeAndPositionChange(mainWindow, requestElectronStore);

  logger.info('[Custom-Protocol] deeplinkingUrl ON READY - URL: ', { deeplinkingUrl });
  logger.info('[Custom-Protocol] deeplinkingUrl ON READY - Get processArgv: ', {
    processArgv: process.argv,
    platform: process.platform,
  });

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

  await handleCheckBlockReplayProgress(mainWindow, launcherConfig.logsPrefix);

  cardanoNode = setupCardanoNode(launcherConfig, mainWindow);

  if (isWatchMode) {
    // Connect to electron-connect server which restarts / reloads windows on file changes
    client.create(mainWindow);
  }

  setStateSnapshotLogChannel.onReceive(
    (data: SetStateSnapshotLogMainResponse) => {
      return Promise.resolve(logStateSnapshot(data));
    }
  );

  generateWalletMigrationReportChannel.onReceive(
    (data: GenerateWalletMigrationReportRendererRequest) => {
      return Promise.resolve(generateWalletMigrationReport(data));
    }
  );

  getStateDirectoryPathChannel.onRequest(() =>
    Promise.resolve(stateDirectoryPath)
  );

  getDesktopDirectoryPathChannel.onRequest(() =>
    Promise.resolve(app.getPath('desktop'))
  );

  getSystemLocaleChannel.onRequest(() => Promise.resolve(systemLocale));

  mainWindow.on('close', async (event) => {
    logger.info(
      'mainWindow received <close> event. Safe exiting Daedalus now.'
    );
    event.preventDefault();
    await safeExit();
  });

  buildAppMenus(mainWindow, cardanoNode, locale, {
    isNavigationEnabled: false,
  });

  await enableApplicationMenuNavigationChannel.onReceive(
    () =>
      new Promise((resolve) => {
        buildAppMenus(mainWindow, cardanoNode, locale, {
          isNavigationEnabled: true,
        });
        resolve();
      })
  );

  await rebuildApplicationMenu.onReceive(
    (data) =>
      new Promise((resolve) => {
        locale = getLocale(network);
        buildAppMenus(mainWindow, cardanoNode, locale, {
          isNavigationEnabled: data.isNavigationEnabled,
        });
        mainWindow.updateTitle(locale);
        resolve();
      })
  );

  // Register custom browser protocol
  if (process.platform === 'win32') {
    logger.info('[Custom-Protocol] Set Windows protocol params: ', {
      platform: process.platform,
    });
    const cardanoLauncherExe = path.resolve(path.dirname(process.execPath), 'cardano-launcher.exe');
    logger.info("[Custom-Protocol] cardano-launcher.exe:", {
      cardanoLauncherExe,
    });
    app.setAsDefaultProtocolClient('web+cardano', cardanoLauncherExe);
    // Check
    const isDefaultProtocolClientSet = app.isDefaultProtocolClient('web+cardano');
    logger.info('[Custom-Protocol] Check isDefaultProtocolClient set Windows: ', {
      isDefaultProtocolClientSet,
    });
  } else {
    logger.info('[Custom-Protocol] Set Mac / Linux protocol params: ', {
      platform: process.platform,
    });
    app.setAsDefaultProtocolClient('web+cardano');
    if (process.platform !== 'linux') {
      childProcess.exec(
        'xdg-mime default Daedalus*.desktop x-scheme-handler/web+cardano'
      );
    }
    // Check
    const isDefaultProtocolClientSet = app.isDefaultProtocolClient('web+cardano');
    logger.info('[Custom-Protocol] isDefaultProtocolClient set Mac / Linux: ', {
      isDefaultProtocolClientSet,
    });
  }

  /* app.on('open-url', (event, url) => {
    event.preventDefault();
    // Check
    const isDefaultProtocolClientSet = app.isDefaultProtocolClient('web+cardano');
    logger.info('[Custom-Protocol] ON READY (open-url) isDefaultProtocolClient: ', {
      isDefaultProtocolClientSet,
      processArgv: process.argv,
    });
    logger.info('[Custom-Protocol] ON READY (open-url) Open params', {
      event,
      url,
    });
    mainWindow.focus();
    try {
      handleCustomProtocol(url, mainWindow);
    } catch (error) {
      logger.info('[Custom-Protocol] ON READY (open-url) Open handler error: ', error);
      throw error;
    }
  }); */

  // Security feature: Prevent creation of new browser windows
  // https://github.com/electron/electron/blob/master/docs/tutorial/security.md#14-disable-or-limit-creation-of-new-windows
  app.on('web-contents-created', (_, contents) => {
    contents.on('new-window', (event, url) => {
      logger.info('[Custom-Protocol] ON new-window', { url });
      // Prevent creation of new BrowserWindows via links / window.open
      event.preventDefault();
      logger.info('Prevented creation of new browser window', { url });
      // Open these links with the default browser
      shell.openExternal(url);
    });
  });

  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    logger.info('app received <before-quit> event. Safe exiting Daedalus now.');
    event.preventDefault(); // prevent Daedalus from quitting immediately

    if (isSelfnode) {
      if (keepLocalClusterRunning || isTest) {
        logger.info(
          'ipcMain: Keeping the local cluster running while exiting Daedalus',
          {
            keepLocalClusterRunning,
          }
        );
        return safeExitWithCode(0);
      }

      const exitSelfnodeDialogOptions = {
        buttons: ['Yes', 'No'],
        type: 'warning',
        title: 'Daedalus is about to close',
        message: 'Do you want to keep the local cluster running?',
        defaultId: 0,
        cancelId: 1,
        noLink: true,
      };
      const { response } = await dialog.showMessageBox(
        mainWindow,
        exitSelfnodeDialogOptions
      );
      if (response === 0) {
        logger.info(
          'ipcMain: Keeping the local cluster running while exiting Daedalus'
        );
        return safeExitWithCode(0);
      }
      logger.info('ipcMain: Exiting local cluster together with Daedalus');
    }

    await safeExit();
  });
};

// Make sure this is the only Daedalus instance running per cluster before doing anything else
const isSingleInstance = app.requestSingleInstanceLock();
logger.info('[Custom-Protocol] isSingleInstance', { isSingleInstance });

app.on('will-finish-launching', () => {
  logger.info('[Custom-Protocol] will-finish-launching');
  // Protocol handler for osx
  app.on('open-url', (event, url) => {
    logger.info('[Custom-Protocol] will-finish-launching - Open URL: ', { url });
    event.preventDefault()
    deeplinkingUrl = `${url}-#1`;
  })
})

// Protocol handler for osx
app.on('open-url', (event, url) => {
  logger.info('[Custom-Protocol] Open URL: ', { url });
  event.preventDefault()
  deeplinkingUrl = `${url}-#2`;
})

if (!isSingleInstance) {
  logger.info('[Custom-Protocol] isSingleInstance - Quit: ', { isSingleInstance });
  if (mainWindow) {
    const exitSelfnodeDialogOptions = {
      buttons: ['Yes', 'No'],
      type: 'warning',
      title: 'Safe EXIT',
      message: 'Safe EXIT - not a single instance',
      defaultId: 0,
      cancelId: 1,
      noLink: true,
    };
    dialog.showMessageBox(
      mainWindow,
      exitSelfnodeDialogOptions
    );
  }
  safeExit();
} else {
  if (mainWindow) {
    const exitSelfnodeDialogOptions2 = {
      buttons: ['Yes', 'No'],
      type: 'warning',
      title: 'It is single instance',
      message: '...',
      defaultId: 0,
      cancelId: 1,
      noLink: true,
    };
    dialog.showMessageBox(
      mainWindow,
      exitSelfnodeDialogOptions2
    );
  }
  logger.info('[Custom-Protocol] isSingleInstance - Continue: ', { isSingleInstance });
  /* app.on('will-finish-launching' , () => {
    logger.info('[Custom-Protocol] will-finish-launching');
    app.on('open-url', (event, url) => {
      event.preventDefault();

      // Check
      const isDefaultProtocolClientSet = app.isDefaultProtocolClient('web+cardano');
      logger.info('[Custom-Protocol] will-finish-launching (open-url) - isDefaultProtocolClient: ', {
        isDefaultProtocolClientSet,
        processArgv: process.argv,
      });

      if (isDefaultProtocolClientSet) {
        logger.info('[Custom-Protocol] will-finish-launching (open-url) - Open params', {
          event,
          url,
        });
      }
    });
  }); */
  app.on('second-instance', () => {
    if (mainWindow) {
    const exitSelfnodeDialogOptions3 = {
      buttons: ['Yes', 'No'],
      type: 'warning',
      title: 'On second instance',
      message: '...',
      defaultId: 0,
      cancelId: 1,
      noLink: true,
    };
    dialog.showMessageBox(
      mainWindow,
      exitSelfnodeDialogOptions3
    );
  }
    logger.info('[Custom-Protocol] isSingleInstance - Is second instance');
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
  app.on('ready', onAppReady);
}
