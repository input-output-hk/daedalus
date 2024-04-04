import os from 'os';
import path from 'path';
import { app, dialog, BrowserWindow, screen, shell } from 'electron';
import type { Event } from 'electron';
import EventEmitter from 'events';
import { WalletSettingsStateEnum } from '../common/ipc/api';
import { requestElectronStore } from './ipc/electronStoreConversation';
import { logger } from './utils/logging';
import {
  setupLogging,
  logSystemInfo,
  logStateSnapshot,
  generateWalletMigrationReport,
} from './utils/setupLogging';
import { handleDiskSpace } from './utils/handleDiskSpace';
import { handleCheckBlockReplayProgress } from './utils/handleCheckBlockReplayProgress';
import { createMainWindow } from './windows/main';
import { installChromeExtensions } from './utils/installChromeExtensions';
import { environment } from './environment';
import mainErrorHandler from './utils/mainErrorHandler';
import {
  launcherConfig,
  pubLogsFolderPath,
  RTS_FLAGS,
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
import type {
  GenerateWalletMigrationReportRendererRequest,
  SetStateSnapshotLogMainResponse,
} from '../common/ipc/api';
import { logUsedVersion } from './utils/logUsedVersion';
import { setStateSnapshotLogChannel } from './ipc/set-log-state-snapshot';
import { generateWalletMigrationReportChannel } from './ipc/generateWalletMigrationReportChannel';
import { pauseActiveDownloads } from './ipc/downloadManagerChannel';
import {
  restoreSavedWindowBounds,
  saveWindowBoundsOnSizeAndPositionChange,
} from './windows/windowBounds';
import {
  getRtsFlagsSettings,
  storeRtsFlagsSettings,
} from './utils/rtsFlagsSettings';
import { toggleRTSFlagsModeChannel } from './ipc/toggleRTSFlagsModeChannel';
import { containsRTSFlags } from './utils/containsRTSFlags';

/* eslint-disable consistent-return */
// Global references to windows to prevent them from being garbage collected
let mainWindow: BrowserWindow;
let cardanoNode: CardanoNode;
const {
  isDev,
  isTest,
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

const safeExit = async () => {
  pauseActiveDownloads();

  if (!cardanoNode || cardanoNode.state === CardanoNodeStates.STOPPED) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', {
      code: 0,
    });
    return safeExitWithCode(0);
  }

  if (cardanoNode.state === CardanoNodeStates.STOPPING) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Daedalus:safeExit: waiting for cardano-node to stop...');
    cardanoNode.exitOnStop();
    return;
  }

  try {
    const pid = cardanoNode.pid || 'null';
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info(`Daedalus:safeExit: stopping cardano-node with PID: ${pid}`, {
      pid,
    });
    await cardanoNode.stop();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', {
      code: 0,
    });
    safeExitWithCode(0);
  } catch (error) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.error('Daedalus:safeExit: cardano-node did not exit correctly', {
      error,
    });
    safeExitWithCode(0);
  }
};

const handleWindowClose = async (event: Event | null | undefined) => {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info('mainWindow received <close> event. Safe exiting Daedalus now.');
  event?.preventDefault();
  await safeExit();
};

const onAppReady = async () => {
  setupLogging();
  await logUsedVersion(
    environment.version,
    path.join(pubLogsFolderPath, 'Daedalus-versions.json')
  );
  const cpu = os.cpus();
  const platformVersion = os.release();
  const ram = JSON.stringify(os.totalmem(), null, 2);
  const startTime = new Date().toISOString();
  // first checks for Japanese locale, otherwise returns english
  const systemLocale = detectSystemLocale();
  const userLocale = getLocale(network);
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
    process.env.DAEDALUS_INSTALL_DIRECTORY,
    process.env.PATH,
  ].join(path.delimiter);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info(`Daedalus is starting at ${startTime}`, {
    startTime,
  });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info('Updating System-info.json file', { ...systemInfo.data });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info(`Current working directory is: ${process.cwd()}`, {
    cwd: process.cwd(),
  });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info('System and user locale', {
    systemLocale,
    userLocale,
  });
  ensureXDGDataIsSet();
  await installChromeExtensions(isDev);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info('Setting up Main Window...');
  mainWindow = createMainWindow(
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
    userLocale,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Electron.Screen' is not assignab... Remove this comment to see the full error message
    () => restoreSavedWindowBounds(screen, requestElectronStore)
  );
  saveWindowBoundsOnSizeAndPositionChange(mainWindow, requestElectronStore);
  const currentRtsFlags = getRtsFlagsSettings(network) || [];
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info(
    `Setting up Cardano Node... with flags: ${JSON.stringify(currentRtsFlags)}`
  );
  cardanoNode = setupCardanoNode(launcherConfig, mainWindow, currentRtsFlags);
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
  buildAppMenus(mainWindow, cardanoNode, userLocale, {
    isNavigationEnabled: false,
    walletSettingsState: WalletSettingsStateEnum.hidden,
  });
  rebuildApplicationMenu.onReceive(
    ({ walletSettingsState, isNavigationEnabled }) =>
      new Promise((resolve) => {
        const locale = getLocale(network);
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
        buildAppMenus(mainWindow, cardanoNode, locale, {
          isNavigationEnabled,
          walletSettingsState,
        });
        // @ts-ignore ts-migrate(2339) FIXME: Property 'updateTitle' does not exist on type 'Bro... Remove this comment to see the full error message
        mainWindow.updateTitle(locale);
        // @ts-ignore ts-migrate(2794) FIXME: Expected 1 arguments, but got 0. Did you forget to... Remove this comment to see the full error message
        resolve();
      })
  );
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
  toggleRTSFlagsModeChannel.onReceive(() => {
    const flagsToSet = containsRTSFlags(currentRtsFlags) ? [] : RTS_FLAGS;
    storeRtsFlagsSettings(environment.network, flagsToSet);
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    return handleWindowClose();
  });
  const handleCheckDiskSpace = handleDiskSpace(mainWindow, cardanoNode);

  const onMainError = (error: string) => {
    if (error.indexOf('ENOSPC') > -1) {
      handleCheckDiskSpace();
      return false;
    }
  };

  mainErrorHandler(onMainError);
  handleCheckBlockReplayProgress(mainWindow, launcherConfig.logsPrefix);
  await handleCheckDiskSpace();

  mainWindow.on('close', handleWindowClose);
  // Security feature: Prevent creation of new browser windows
  // https://github.com/electron/electron/blob/master/docs/tutorial/security.md#14-disable-or-limit-creation-of-new-windows
  app.on('web-contents-created', (_, contents) => {
    contents.setWindowOpenHandler((details) => {
      const { url } = details;
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.info('Prevented creation of new browser window', {
        url,
      });
      // Open these links with the default browser
      shell.openExternal(url);
      // Prevent creation of new BrowserWindows via links / window.open
      return { action: 'deny' };
    });
  });
  // Wait for controlled cardano-node shutdown before quitting the app
  app.on('before-quit', async (event) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('app received <before-quit> event. Safe exiting Daedalus now.');
    event.preventDefault(); // prevent Daedalus from quitting immediately

    if (isSelfnode) {
      if (keepLocalClusterRunning || isTest) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
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
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logger.info(
          'ipcMain: Keeping the local cluster running while exiting Daedalus'
        );
        return safeExitWithCode(0);
      }

      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.info('ipcMain: Exiting local cluster together with Daedalus');
    }

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
