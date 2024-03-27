'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.environment = exports.RECOMMENDED_RAM_IN_BYTES = void 0;
const os_1 = __importDefault(require('os'));
const lodash_1 = require('lodash');
const package_json_1 = __importDefault(require('../../package.json'));
const environment_types_1 = require('../common/types/environment.types');
const environmentCheckers_1 = require('../common/utils/environmentCheckers');
const version = `${package_json_1.default.version}`;
// Daedalus requires minimum 16 gigabytes of RAM, but some devices having 16 GB
// actually have a slightly smaller RAM size (eg. 15.99 GB), therefore we used 15 GB threshold
//
// TODO figure out better place for it - can't import from config.js as it would be a circular dep
// https://input-output.atlassian.net/browse/DDW-928
exports.RECOMMENDED_RAM_IN_BYTES = 15 * 1024 * 1024 * 1024;
/* ==================================================================
=                           Evaluations                             =
================================================================== */
// environment variables
const CURRENT_NODE_ENV =
  process.env.NODE_ENV || environment_types_1.DEVELOPMENT;
const NETWORK = (0, environmentCheckers_1.evaluateNetwork)(process.env.NETWORK);
const isDev = (0, environmentCheckers_1.checkIsDev)(CURRENT_NODE_ENV);
const isTest = (0, environmentCheckers_1.checkIsTest)(CURRENT_NODE_ENV);
const isProduction = (0, environmentCheckers_1.checkIsProduction)(
  CURRENT_NODE_ENV
);
const isMainnet = (0, environmentCheckers_1.checkIsMainnet)(NETWORK);
const isStaging = (0, environmentCheckers_1.checkIsStaging)(NETWORK);
const isTestnet = (0, environmentCheckers_1.checkIsTestnet)(NETWORK);
const isAlonzoPurple = (0, environmentCheckers_1.checkIsAlonzoPurple)(NETWORK);
const isVasilDev = (0, environmentCheckers_1.checkIsVasilDev)(NETWORK);
const isPreprod = (0, environmentCheckers_1.checkIsPreprod)(NETWORK);
const isPreview = (0, environmentCheckers_1.checkIsPreview)(NETWORK);
const isShelleyQA = (0, environmentCheckers_1.checkIsShelleyQA)(NETWORK);
const isSelfnode = (0, environmentCheckers_1.checkIsSelfnode)(NETWORK);
const isDevelopment = (0, environmentCheckers_1.checkIsDevelopment)(NETWORK);
const analyticsFeatureEnabled = true;
const keepLocalClusterRunning = process.env.KEEP_LOCAL_CLUSTER_RUNNING;
const CARDANO_WALLET_VERSION = process.env.CARDANO_WALLET_VERSION || 'dev';
const CARDANO_NODE_VERSION = process.env.CARDANO_NODE_VERSION || 'dev';
const mainProcessID = (0, lodash_1.get)(process, 'ppid', '-');
const rendererProcessID = process.pid;
const PLATFORM = os_1.default.platform();
const PLATFORM_VERSION = os_1.default.release();
const OS = environment_types_1.OS_NAMES[PLATFORM] || PLATFORM;
const cpu = os_1.default.cpus();
const ram = os_1.default.totalmem();
const hasMetHardwareRequirements = ram >= exports.RECOMMENDED_RAM_IN_BYTES;
const isBlankScreenFixActive = (0, lodash_1.includes)(
  process.argv.slice(1),
  '--safe-mode'
);
const BUILD = process.env.BUILD_REV_SHORT || 'dev';
const BUILD_NUMBER = (0, lodash_1.uniq)([CARDANO_WALLET_VERSION, BUILD]).join(
  '.'
);
const INSTALLER_VERSION = (0, lodash_1.uniq)([
  CARDANO_WALLET_VERSION,
  BUILD,
]).join('.');
const isMacOS = (0, environmentCheckers_1.checkIsMacOS)(PLATFORM);
const isWindows = (0, environmentCheckers_1.checkIsWindows)(PLATFORM);
const isLinux = (0, environmentCheckers_1.checkIsLinux)(PLATFORM);
/* ==================================================================
=                       Compose environment                         =
================================================================== */
// @ts-ignore ts-migrate(2322) FIXME: Type '{ network: string; apiVersion: string; nodeV... Remove this comment to see the full error message
exports.environment = Object.assign(
  {},
  {
    network: NETWORK,
    apiVersion: CARDANO_WALLET_VERSION,
    nodeVersion: CARDANO_NODE_VERSION,
    current: CURRENT_NODE_ENV,
    isDev,
    isTest,
    isProduction,
    isMainnet,
    isStaging,
    isTestnet,
    isAlonzoPurple,
    isVasilDev,
    isPreprod,
    isPreview,
    isShelleyQA,
    isSelfnode,
    isDevelopment,
    build: BUILD,
    buildNumber: BUILD_NUMBER,
    platform: PLATFORM,
    platformVersion: PLATFORM_VERSION,
    mainProcessID,
    rendererProcessID,
    os: OS,
    cpu,
    ram,
    installerVersion: INSTALLER_VERSION,
    version,
    isWindows,
    isMacOS,
    isLinux,
    isBlankScreenFixActive,
    keepLocalClusterRunning,
    hasMetHardwareRequirements,
    analyticsFeatureEnabled,
    catalystApiUrlOverride: process.env.CATALYST_API_URL_OVERRIDE,
    votingVisibleOverride: process.env.VOTING_VISIBLE_OVERRIDE === 'true',
  },
  process.env
);
//# sourceMappingURL=environment.js.map
