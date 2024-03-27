'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.RTS_FLAGS = exports.MOCK_TOKEN_METADATA_SERVER_PORT = exports.MOCK_TOKEN_METADATA_SERVER_URL = exports.MINIMUM_AMOUNT_OF_RAM_FOR_RTS_FLAGS = exports.FALLBACK_TOKEN_METADATA_SERVER_URL = exports.DISK_SPACE_CHECK_TIMEOUT = exports.DISK_SPACE_RECOMMENDED_PERCENTAGE = exports.DISK_SPACE_CHECK_SHORT_INTERVAL = exports.DISK_SPACE_CHECK_MEDIUM_INTERVAL = exports.DISK_SPACE_CHECK_LONG_INTERVAL = exports.DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL = exports.DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE = exports.DISK_SPACE_REQUIRED = exports.NODE_UPDATE_TIMEOUT = exports.NODE_KILL_TIMEOUT = exports.NODE_SHUTDOWN_TIMEOUT = exports.NODE_STARTUP_MAX_RETRIES = exports.NODE_STARTUP_TIMEOUT = exports.MAX_LAUNCHER_LOGS_ALLOWED = exports.MAX_WALLET_LOGS_ALLOWED = exports.MAX_NODE_LOGS_ALLOWED = exports.ALLOWED_LAUNCHER_LOGS = exports.ALLOWED_WALLET_LOGS = exports.ALLOWED_NODE_LOGS = exports.ALLOWED_LOGS = exports.buildLabel = exports.stateDirectoryPath = exports.pubLogsFolderPath = exports.appLogsFolderPath = exports.smashUrl = exports.isFlight = exports.logsPrefix = exports.legacyStateDir = exports.stateDir = exports.nodeImplementation = exports.cluster = exports.launcherConfig = exports.windowOptions = exports.MIN_WINDOW_CONTENT_HEIGHT = exports.MIN_WINDOW_CONTENT_WIDTH = exports.DEFAULT_WINDOW_HEIGHT = exports.DEFAULT_WINDOW_WIDTH = void 0;
const path_1 = __importDefault(require('path'));
const electron_1 = require('electron');
const environment_1 = require('./environment');
const config_1 = require('./utils/config');
const environmentCheckers_1 = require('../common/utils/environmentCheckers');
const {
  isTest,
  isProduction,
  isBlankScreenFixActive,
  current,
  build,
  network,
  version,
} = environment_1.environment;
// Make sure Daedalus is started with required configuration
const { LAUNCHER_CONFIG } = process.env;
const isStartedByLauncher = !!LAUNCHER_CONFIG;
const isWindows = process.platform === 'win32';
if (!isStartedByLauncher) {
  const dialogTitle = 'Daedalus improperly started!';
  let dialogMessage;
  if (isProduction) {
    dialogMessage = isWindows
      ? 'Please start Daedalus using the icon in the Windows start menu or using Daedalus icon on your desktop.'
      : 'Daedalus was launched without needed configuration. Please start Daedalus using the shortcut provided by the installer.';
  } else {
    dialogMessage =
      'Daedalus should be started using nix-shell. Find more details here: https://github.com/input-output-hk/daedalus/blob/develop/README.md';
  }
  try {
    // app may not be available at this moment so we need to use try-catch
    electron_1.dialog.showErrorBox(dialogTitle, dialogMessage);
    electron_1.app.exit(1);
  } catch (e) {
    throw new Error(`${dialogTitle}\n\n${dialogMessage}\n`);
  }
}
exports.DEFAULT_WINDOW_WIDTH = 1150;
exports.DEFAULT_WINDOW_HEIGHT = 870;
exports.MIN_WINDOW_CONTENT_WIDTH = 905;
exports.MIN_WINDOW_CONTENT_HEIGHT = 564;
exports.windowOptions = {
  show: false,
  width: exports.DEFAULT_WINDOW_WIDTH,
  height: exports.DEFAULT_WINDOW_HEIGHT,
  webPreferences: {
    nodeIntegration: isTest,
    webviewTag: false,
    enableRemoteModule: isTest,
    preload: path_1.default.join(__dirname, './preload.js'),
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ nodeIntegration: boolean; webviewTag: fals... Remove this comment to see the full error message
    additionalArguments: isBlankScreenFixActive ? ['--safe-mode'] : [],
  },
  useContentSize: true,
};
exports.launcherConfig = (0, config_1.readLauncherConfig)(LAUNCHER_CONFIG);
(exports.cluster = exports.launcherConfig.cluster),
  (exports.nodeImplementation = exports.launcherConfig.nodeImplementation),
  (exports.stateDir = exports.launcherConfig.stateDir),
  (exports.legacyStateDir = exports.launcherConfig.legacyStateDir),
  (exports.logsPrefix = exports.launcherConfig.logsPrefix),
  (exports.isFlight = exports.launcherConfig.isFlight),
  (exports.smashUrl = exports.launcherConfig.smashUrl);
exports.appLogsFolderPath = exports.logsPrefix;
exports.pubLogsFolderPath = path_1.default.join(
  exports.appLogsFolderPath,
  'pub'
);
exports.stateDirectoryPath = exports.stateDir;
exports.buildLabel = (0, environmentCheckers_1.getBuildLabel)(
  build,
  network,
  current,
  exports.isFlight,
  version
);
// Logging config
exports.ALLOWED_LOGS = [
  'Daedalus.json',
  'System-info.json',
  'Daedalus-versions.json',
  'State-snapshot.json',
  'Wallet-migration-report.json',
  'cardano-wallet.log',
  'node.log',
];
exports.ALLOWED_NODE_LOGS = new RegExp(/(node.log-)(\d{14}$)/);
exports.ALLOWED_WALLET_LOGS = new RegExp(/(cardano-wallet.log-)(\d{14}$)/);
exports.ALLOWED_LAUNCHER_LOGS = new RegExp(/(launcher-)(\d{14}$)/);
exports.MAX_NODE_LOGS_ALLOWED = 3;
exports.MAX_WALLET_LOGS_ALLOWED = 3;
exports.MAX_LAUNCHER_LOGS_ALLOWED = 3;
// CardanoNode config
exports.NODE_STARTUP_TIMEOUT = 5000;
exports.NODE_STARTUP_MAX_RETRIES = 5;
exports.NODE_SHUTDOWN_TIMEOUT = isTest ? 5000 : 5 * 60 * 1000; // 5 minutes | unit: milliseconds
exports.NODE_KILL_TIMEOUT = isTest ? 5000 : 5 * 60 * 1000; // 5 minutes | unit: milliseconds
exports.NODE_UPDATE_TIMEOUT = isTest ? 10000 : 5 * 60 * 1000; // 5 minutes | unit: milliseconds
exports.DISK_SPACE_REQUIRED = 4 * 1073741274; // 4 GB | unit: bytes
exports.DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE = 10; // 10% of the available disk space
exports.DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL = Number.MAX_SAFE_INTEGER; // Maximum interval
exports.DISK_SPACE_CHECK_LONG_INTERVAL = 10 * 60 * 1000; // 10 minutes | unit: milliseconds
exports.DISK_SPACE_CHECK_MEDIUM_INTERVAL = 60 * 1000; // 1 minute | unit: milliseconds
exports.DISK_SPACE_CHECK_SHORT_INTERVAL = isTest ? 2000 : 10 * 1000; // 10 seconds | unit: milliseconds
exports.DISK_SPACE_RECOMMENDED_PERCENTAGE = 15; // 15% of the total disk space
exports.DISK_SPACE_CHECK_TIMEOUT = 9 * 1000; // Timeout for checking disks pace
// Used if token metadata server URL is not defined in launcher config
exports.FALLBACK_TOKEN_METADATA_SERVER_URL =
  'https://metadata.cardano-testnet.iohkdev.io';
exports.MINIMUM_AMOUNT_OF_RAM_FOR_RTS_FLAGS = 16 * 1024 * 1024 * 1024; // 16gb RAM
// Used by mock-token-metadata-server
// “localhost” breaks under new electron, which prefers ::1 (IPv6)
exports.MOCK_TOKEN_METADATA_SERVER_URL = 'http://127.0.0.1';
exports.MOCK_TOKEN_METADATA_SERVER_PORT =
  process.env.MOCK_TOKEN_METADATA_SERVER_PORT || 0;
exports.RTS_FLAGS = ['-c'];
//# sourceMappingURL=config.js.map
