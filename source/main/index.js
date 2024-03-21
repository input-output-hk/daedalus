'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const os_1 = __importDefault(require('os'));
const path_1 = __importDefault(require('path'));
const electron_1 = require('electron');
const events_1 = __importDefault(require('events'));
const api_1 = require('../common/ipc/api');
const electronStoreConversation_1 = require('./ipc/electronStoreConversation');
const logging_1 = require('./utils/logging');
const setupLogging_1 = require('./utils/setupLogging');
const handleDiskSpace_1 = require('./utils/handleDiskSpace');
const handleCheckBlockReplayProgress_1 = require('./utils/handleCheckBlockReplayProgress');
const main_1 = require('./windows/main');
const installChromeExtensions_1 = require('./utils/installChromeExtensions');
const environment_1 = require('./environment');
const mainErrorHandler_1 = __importDefault(require('./utils/mainErrorHandler'));
const config_1 = require('./config');
const setup_1 = require('./cardano/setup');
const safeExitWithCode_1 = require('./utils/safeExitWithCode');
const buildAppMenus_1 = require('./utils/buildAppMenus');
const getLocale_1 = require('./utils/getLocale');
const detectSystemLocale_1 = require('./utils/detectSystemLocale');
const config_2 = require('./cardano/config');
const rebuild_application_menu_1 = require('./ipc/rebuild-application-menu');
const getStateDirectoryPathChannel_1 = require('./ipc/getStateDirectoryPathChannel');
const getDesktopDirectoryPathChannel_1 = require('./ipc/getDesktopDirectoryPathChannel');
const getSystemLocaleChannel_1 = require('./ipc/getSystemLocaleChannel');
const cardano_node_types_1 = require('../common/types/cardano-node.types');
const logUsedVersion_1 = require('./utils/logUsedVersion');
const set_log_state_snapshot_1 = require('./ipc/set-log-state-snapshot');
const generateWalletMigrationReportChannel_1 = require('./ipc/generateWalletMigrationReportChannel');
const downloadManagerChannel_1 = require('./ipc/downloadManagerChannel');
const windowBounds_1 = require('./windows/windowBounds');
const rtsFlagsSettings_1 = require('./utils/rtsFlagsSettings');
const toggleRTSFlagsModeChannel_1 = require('./ipc/toggleRTSFlagsModeChannel');
const containsRTSFlags_1 = require('./utils/containsRTSFlags');
/* eslint-disable consistent-return */
// Global references to windows to prevent them from being garbage collected
let mainWindow;
let cardanoNode;
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
} = environment_1.environment;
if (isBlankScreenFixActive) {
  // Run "console.log(JSON.stringify(daedalus.stores.app.gpuStatus, null, 2))"
  // in DevTools JavaScript console to see if the flag is active
  electron_1.app.disableHardwareAcceleration();
}
// Increase maximum event listeners to avoid IPC channel stalling
// (1/2) this line increases the limit for the main process
events_1.default.defaultMaxListeners = 100; // Default: 10
const safeExit = async () => {
  (0, downloadManagerChannel_1.pauseActiveDownloads)();
  if (
    !cardanoNode ||
    cardanoNode.state === cardano_node_types_1.CardanoNodeStates.STOPPED
  ) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Daedalus:safeExit: exiting Daedalus with code 0', {
      code: 0,
    });
    return (0, safeExitWithCode_1.safeExitWithCode)(0);
  }
  if (cardanoNode.state === cardano_node_types_1.CardanoNodeStates.STOPPING) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      'Daedalus:safeExit: waiting for cardano-node to stop...'
    );
    cardanoNode.exitOnStop();
    return;
  }
  try {
    const pid = cardanoNode.pid || 'null';
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      `Daedalus:safeExit: stopping cardano-node with PID: ${pid}`,
      {
        pid,
      }
    );
    await cardanoNode.stop();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Daedalus:safeExit: exiting Daedalus with code 0', {
      code: 0,
    });
    (0, safeExitWithCode_1.safeExitWithCode)(0);
  } catch (error) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.error(
      'Daedalus:safeExit: cardano-node did not exit correctly',
      {
        error,
      }
    );
    (0, safeExitWithCode_1.safeExitWithCode)(0);
  }
};
const handleWindowClose = async (event) => {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info(
    'mainWindow received <close> event. Safe exiting Daedalus now.'
  );
  event?.preventDefault();
  await safeExit();
};
const onAppReady = async () => {
  (0, setupLogging_1.setupLogging)();
  await (0, logUsedVersion_1.logUsedVersion)(
    environment_1.environment.version,
    path_1.default.join(config_1.pubLogsFolderPath, 'Daedalus-versions.json')
  );
  const cpu = os_1.default.cpus();
  const platformVersion = os_1.default.release();
  const ram = JSON.stringify(os_1.default.totalmem(), null, 2);
  const startTime = new Date().toISOString();
  // first checks for Japanese locale, otherwise returns english
  const systemLocale = (0, detectSystemLocale_1.detectSystemLocale)();
  const userLocale = (0, getLocale_1.getLocale)(network);
  const systemInfo = (0, setupLogging_1.logSystemInfo)({
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
  ].join(path_1.default.delimiter);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info(`Daedalus is starting at ${startTime}`, {
    startTime,
  });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info('Updating System-info.json file', {
    ...systemInfo.data,
  });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info(`Current working directory is: ${process.cwd()}`, {
    cwd: process.cwd(),
  });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info('System and user locale', {
    systemLocale,
    userLocale,
  });
  (0, config_2.ensureXDGDataIsSet)();
  await (0, installChromeExtensions_1.installChromeExtensions)(isDev);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info('Setting up Main Window...');
  mainWindow = (0, main_1.createMainWindow)(
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
    userLocale,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Electron.Screen' is not assignab... Remove this comment to see the full error message
    () =>
      (0, windowBounds_1.restoreSavedWindowBounds)(
        electron_1.screen,
        electronStoreConversation_1.requestElectronStore
      )
  );
  (0, windowBounds_1.saveWindowBoundsOnSizeAndPositionChange)(
    mainWindow,
    electronStoreConversation_1.requestElectronStore
  );
  const currentRtsFlags =
    (0, rtsFlagsSettings_1.getRtsFlagsSettings)(network) || [];
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info(
    `Setting up Cardano Node... with flags: ${JSON.stringify(currentRtsFlags)}`
  );
  cardanoNode = (0, setup_1.setupCardanoNode)(
    config_1.launcherConfig,
    mainWindow,
    currentRtsFlags
  );
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
  (0, buildAppMenus_1.buildAppMenus)(mainWindow, cardanoNode, userLocale, {
    isNavigationEnabled: false,
    walletSettingsState: api_1.WalletSettingsStateEnum.hidden,
  });
  rebuild_application_menu_1.rebuildApplicationMenu.onReceive(
    ({ walletSettingsState, isNavigationEnabled }) =>
      new Promise((resolve) => {
        const locale = (0, getLocale_1.getLocale)(network);
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
        (0, buildAppMenus_1.buildAppMenus)(mainWindow, cardanoNode, locale, {
          isNavigationEnabled,
          walletSettingsState,
        });
        // @ts-ignore ts-migrate(2339) FIXME: Property 'updateTitle' does not exist on type 'Bro... Remove this comment to see the full error message
        mainWindow.updateTitle(locale);
        // @ts-ignore ts-migrate(2794) FIXME: Expected 1 arguments, but got 0. Did you forget to... Remove this comment to see the full error message
        resolve();
      })
  );
  set_log_state_snapshot_1.setStateSnapshotLogChannel.onReceive((data) => {
    return Promise.resolve((0, setupLogging_1.logStateSnapshot)(data));
  });
  generateWalletMigrationReportChannel_1.generateWalletMigrationReportChannel.onReceive(
    (data) => {
      return Promise.resolve(
        (0, setupLogging_1.generateWalletMigrationReport)(data)
      );
    }
  );
  getStateDirectoryPathChannel_1.getStateDirectoryPathChannel.onRequest(() =>
    Promise.resolve(config_1.stateDirectoryPath)
  );
  getDesktopDirectoryPathChannel_1.getDesktopDirectoryPathChannel.onRequest(
    () => Promise.resolve(electron_1.app.getPath('desktop'))
  );
  getSystemLocaleChannel_1.getSystemLocaleChannel.onRequest(() =>
    Promise.resolve(systemLocale)
  );
  toggleRTSFlagsModeChannel_1.toggleRTSFlagsModeChannel.onReceive(() => {
    const flagsToSet = (0, containsRTSFlags_1.containsRTSFlags)(currentRtsFlags)
      ? []
      : config_1.RTS_FLAGS;
    (0, rtsFlagsSettings_1.storeRtsFlagsSettings)(
      environment_1.environment.network,
      flagsToSet
    );
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    return handleWindowClose();
  });
  const handleCheckDiskSpace = (0, handleDiskSpace_1.handleDiskSpace)(
    mainWindow,
    cardanoNode
  );
  const onMainError = (error) => {
    if (error.indexOf('ENOSPC') > -1) {
      handleCheckDiskSpace();
      return false;
    }
  };
  (0, mainErrorHandler_1.default)(onMainError);
  (0, handleCheckBlockReplayProgress_1.handleCheckBlockReplayProgress)(
    mainWindow,
    config_1.launcherConfig.logsPrefix
  );
  await handleCheckDiskSpace();
  mainWindow.on('close', handleWindowClose);
  // Security feature: Prevent creation of new browser windows
  // https://github.com/electron/electron/blob/master/docs/tutorial/security.md#14-disable-or-limit-creation-of-new-windows
  electron_1.app.on('web-contents-created', (_, contents) => {
    contents.setWindowOpenHandler((details) => {
      const { url } = details;
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info('Prevented creation of new browser window', {
        url,
      });
      // Open these links with the default browser
      electron_1.shell.openExternal(url);
      // Prevent creation of new BrowserWindows via links / window.open
      return { action: 'deny' };
    });
  });
  // Wait for controlled cardano-node shutdown before quitting the app
  electron_1.app.on('before-quit', async (event) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      'app received <before-quit> event. Safe exiting Daedalus now.'
    );
    event.preventDefault(); // prevent Daedalus from quitting immediately
    if (isSelfnode) {
      if (keepLocalClusterRunning || isTest) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          'ipcMain: Keeping the local cluster running while exiting Daedalus',
          {
            keepLocalClusterRunning,
          }
        );
        return (0, safeExitWithCode_1.safeExitWithCode)(0);
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
      const { response } = await electron_1.dialog.showMessageBox(
        mainWindow,
        exitSelfnodeDialogOptions
      );
      if (response === 0) {
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
        logging_1.logger.info(
          'ipcMain: Keeping the local cluster running while exiting Daedalus'
        );
        return (0, safeExitWithCode_1.safeExitWithCode)(0);
      }
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        'ipcMain: Exiting local cluster together with Daedalus'
      );
    }
    await safeExit();
  });
};
// Make sure this is the only Daedalus instance running per cluster before doing anything else
const isSingleInstance = electron_1.app.requestSingleInstanceLock();
if (!isSingleInstance) {
  electron_1.app.quit();
} else {
  electron_1.app.on('second-instance', () => {
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
  electron_1.app.on('ready', onAppReady);
}
//# sourceMappingURL=index.js.map
