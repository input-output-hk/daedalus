import os from 'os';
import path from 'path';
import net from 'net';
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
import { createMainWindow } from './windows/main';
import { installChromeExtensions } from './utils/installChromeExtensions';
import { environment } from './environment';
import mainErrorHandler from './utils/mainErrorHandler';
import {
  pubLogsFolderPath,
  RTS_FLAGS,
  stateDirectoryPath,
  launcherConfig,
} from './config';
import { backendLifecycle } from './BackendLifecycle';
import { safeExitWithCode } from './utils/safeExitWithCode';
import { buildAppMenus } from './utils/buildAppMenus';
import { getLocale } from './utils/getLocale';
import { detectSystemLocale } from './utils/detectSystemLocale';
import { rebuildApplicationMenu } from './ipc/rebuild-application-menu';
import { getStateDirectoryPathChannel } from './ipc/getStateDirectoryPathChannel';
import { getDesktopDirectoryPathChannel } from './ipc/getDesktopDirectoryPathChannel';
import { getSystemLocaleChannel } from './ipc/getSystemLocaleChannel';
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
import { parseDeviceScaleFactor } from './utils/parseDeviceScaleFactor';
/* eslint-disable consistent-return */
// Global references to windows to prevent them from being garbage collected
let mainWindow: BrowserWindow;
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

// Chromium sizes windows in device-independent pixels, so the minimum content
// size set in `createMainWindow` is multiplied by the device scale factor that
// Chromium detects. On a HiDPI display that can make the window too large to
// fit the screen at all. This has to be applied before the app is ready, like
// `disableHardwareAcceleration` above.
const deviceScaleFactor = parseDeviceScaleFactor(
  process.env.DAEDALUS_DEVICE_SCALE_FACTOR
);

if (deviceScaleFactor !== null) {
  app.commandLine.appendSwitch(
    'force-device-scale-factor',
    String(deviceScaleFactor)
  );
}

// Increase maximum event listeners to avoid IPC channel stalling
// (1/2) this line increases the limit for the main process
EventEmitter.defaultMaxListeners = 100; // Default: 10

const safeExit = async () => {
  pauseActiveDownloads();

  const exitCode =
    (mainWindow as any).daedalusExitCode !== undefined
      ? (mainWindow as any).daedalusExitCode
      : 0;

  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info(`Daedalus:safeExit: exiting Daedalus with code ${exitCode}`, {
    code: exitCode,
  });
  return safeExitWithCode(exitCode);
};

const handleWindowClose = async (event?: Event | null) => {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info('mainWindow received <close> event. Safe exiting Daedalus now.');
  event?.preventDefault();
  await safeExit();
};

function getFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.listen(0, '127.0.0.1', () => {
      const port = (srv.address() as net.AddressInfo).port;
      srv.close(() => resolve(port));
    });
    srv.on('error', reject);
  });
}

function buildNodeArgs(
  stateDir: string,
  nodePort: number,
  nodeConfig: import('./config').NodeConfig
): string[] {
  const { configFile, topologyFile } = nodeConfig.network;
  const args = [
    'run',
    '--socket-path',
    process.platform === 'win32'
      ? '\\\\.\\pipe\\cardano-node.socket'
      : 'cardano-node.socket',
    '--topology', topologyFile,
    '--database-path', 'chain',
    '--port', String(nodePort),
    '--config', configFile,
  ];
  if (nodeConfig.signingKey) args.push('--signing-key', nodeConfig.signingKey);
  if (nodeConfig.delegationCertificate) args.push('--delegation-certificate', nodeConfig.delegationCertificate);
  args.push('+RTS', '-N', '-RTS');
  return args;
}

function buildWalletArgs(
  stateDir: string,
  walletPort: number,
  tlsPath: string,
  syncTolerance: string,
  isStaging: boolean,
  metadataUrl: string | undefined,
  nodeConfig: import('./config').NodeConfig
): string[] {
  const socketPath =
    process.platform === 'win32'
      ? '\\\\.\\pipe\\cardano-node.socket'
      : path.join(stateDir, 'cardano-node.socket');
  const walletDb = path.join(stateDir, 'wallets');
  const syncToleranceSecs = parseInt(syncTolerance.replace('s', ''), 10);
  const configDir = path.dirname(nodeConfig.network.configFile);

  const args = [
    'serve', '+RTS', '-N', '-RTS',
    '--port', String(walletPort),
    '--database', walletDb,
    '--tls-ca-cert', path.join(tlsPath, 'server/ca.crt'),
    '--tls-sv-cert', path.join(tlsPath, 'server/server.crt'),
    '--tls-sv-key', path.join(tlsPath, 'server/server.key'),
    '--node-socket', socketPath,
  ];

  if (isStaging) {
    args.push('--mainnet');
  } else {
    args.push('--testnet', path.join(configDir, 'genesis-byron.json'));
  }

  if (!Number.isNaN(syncToleranceSecs)) {
    args.push('--sync-tolerance', `${syncToleranceSecs}s`);
  }

  args.push('--token-metadata-server', metadataUrl ?? 'https://tokens.cardano.org');
  return args;
}

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
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info('GPU hardware acceleration', {
    disabled: app.commandLine.hasSwitch('disable-gpu'),
  });
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
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
  buildAppMenus(mainWindow,userLocale, {
    isNavigationEnabled: false,
    walletSettingsState: WalletSettingsStateEnum.hidden,
  });
  rebuildApplicationMenu.onReceive(
    ({ walletSettingsState, isNavigationEnabled }) =>
      new Promise((resolve) => {
        const locale = getLocale(network);
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'unknown' is not assignable to pa... Remove this comment to see the full error message
        buildAppMenus(mainWindow,locale, {
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
  const handleCheckDiskSpace = handleDiskSpace(mainWindow);

  const onMainError = (error: string) => {
    if (error.indexOf('ENOSPC') > -1) {
      handleCheckDiskSpace();
      return false;
    }
  };

  mainErrorHandler(onMainError);
  await handleCheckDiskSpace();

  // Start watchdog
  backendLifecycle.setWindowProvider(() => mainWindow);
  const {
    watchdogBin, nodeBin, walletBin, logsPrefix,
    nodeConfig, tlsPath, syncTolerance, isStaging, metadataUrl,
    mithrilBin, snapshotConverterBin, mithrilConverterConfig,
    mithrilAggregatorUrl, mithrilGenesisVkey, mithrilAncillaryVkey,
  } = launcherConfig;
  const socketPath = process.platform === 'win32'
    ? '\\\\.\\pipe\\cardano-node.socket'
    : path.join(stateDirectoryPath, 'cardano-node.socket');
  const defaultChainPath = path.join(stateDirectoryPath, 'chain');
  // Load persisted custom chain path from electron-store
  const customChainPath = (requestElectronStore({
    type: 'get',
    key: 'CUSTOM-CHAIN-PATH',
  }) as string | undefined) ?? null;
  const effectiveChainPath = customChainPath
    ? path.join(customChainPath, 'chain')
    : defaultChainPath;
  const [nodePort, walletPort] = await Promise.all([getFreePort(), getFreePort()]);
  const nodeArgs = buildNodeArgs(stateDirectoryPath, nodePort, nodeConfig);
  const walletArgs = buildWalletArgs(stateDirectoryPath, walletPort, tlsPath, syncTolerance, isStaging, metadataUrl, nodeConfig);
  backendLifecycle.setTlsPath(tlsPath);
  backendLifecycle.setChainPaths(defaultChainPath, customChainPath);
  backendLifecycle.start(watchdogBin, {
    node: { exe: nodeBin, args: nodeArgs, state_dir: stateDirectoryPath, socket_path: socketPath },
    wallet: { exe: walletBin, args: walletArgs, state_dir: stateDirectoryPath, api_port: walletPort },
    node_log_file: path.join(logsPrefix, 'node.log'),
    wallet_log_file: path.join(logsPrefix, 'cardano-wallet.log'),
    ...(mithrilBin && mithrilAggregatorUrl && mithrilGenesisVkey ? {
      mithril: {
        mithril_bin: mithrilBin,
        snapshot_converter_bin: snapshotConverterBin ?? '',
        converter_config: mithrilConverterConfig ?? '',
        aggregator_url: mithrilAggregatorUrl,
        genesis_vkey: mithrilGenesisVkey,
        ancillary_vkey: mithrilAncillaryVkey,
        state_dir: stateDirectoryPath,
        chain_path: effectiveChainPath,
      },
    } : {}),
  });

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
    await backendLifecycle.stop();

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
        type: 'warning' as const,
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
