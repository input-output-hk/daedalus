// @flow
import os from 'os';
import { get, includes, uniq } from 'lodash';
import { version } from '../../package.json';
import type { Environment } from '../common/types/environment.types';
import { DEVELOPMENT, OS_NAMES } from '../common/types/environment.types';
import {
  evaluateNetwork,
  checkIsAlonzoPurple,
  checkIsShelleyQA,
  checkIsDev,
  checkIsTest,
  checkIsProduction,
  checkIsMainnet,
  checkIsStaging,
  checkIsTestnet,
  checkIsSelfnode,
  checkIsDevelopment,
  checkIsMacOS,
  checkIsWindows,
  checkIsLinux,
} from '../common/utils/environmentCheckers';

// TODO figure out better place for it - can't import from config.js as it would be a circular dep
export const RECOMMENDED_RAM_IN_BYTES = 16 * 1024 * 1024 * 1024;

/* ==================================================================
=                           Evaluations                             =
================================================================== */

// environment variables
const CURRENT_NODE_ENV = process.env.NODE_ENV || DEVELOPMENT;
const NETWORK = evaluateNetwork(process.env.NETWORK);
const isDev = checkIsDev(CURRENT_NODE_ENV);
const isTest = checkIsTest(CURRENT_NODE_ENV);
const isProduction = checkIsProduction(CURRENT_NODE_ENV);
const isMainnet = checkIsMainnet(NETWORK);
const isStaging = checkIsStaging(NETWORK);
const isTestnet = checkIsTestnet(NETWORK);
const isAlonzoPurple = checkIsAlonzoPurple(NETWORK);
const isShelleyQA = checkIsShelleyQA(NETWORK);
const isSelfnode = checkIsSelfnode(NETWORK);
const isDevelopment = checkIsDevelopment(NETWORK);
const isWatchMode = process.env.IS_WATCH_MODE;
const keepLocalClusterRunning = process.env.KEEP_LOCAL_CLUSTER_RUNNING;
const API_VERSION = process.env.API_VERSION || 'dev';
const NODE_VERSION = '1.33.0'; // TODO: pick up this value from process.env
const mainProcessID = get(process, 'ppid', '-');
const rendererProcessID = process.pid;
const PLATFORM = os.platform();
const PLATFORM_VERSION = os.release();
const OS = OS_NAMES[PLATFORM] || PLATFORM;
const cpu = os.cpus();
const ram = os.totalmem();
const hasMetHardwareRequirements = ram >= RECOMMENDED_RAM_IN_BYTES;
const isBlankScreenFixActive = includes(process.argv.slice(1), '--safe-mode');
const BUILD = process.env.BUILD_NUMBER || 'dev';
const BUILD_NUMBER = uniq([API_VERSION, BUILD]).join('.');
const INSTALLER_VERSION = uniq([API_VERSION, BUILD]).join('.');
const MOBX_DEV_TOOLS = process.env.MOBX_DEV_TOOLS || false;
const isMacOS = checkIsMacOS(PLATFORM);
const isWindows = checkIsWindows(PLATFORM);
const isLinux = checkIsLinux(PLATFORM);

/* ==================================================================
=                       Compose environment                         =
================================================================== */

export const environment: Environment = Object.assign(
  {},
  {
    network: NETWORK,
    apiVersion: API_VERSION,
    nodeVersion: NODE_VERSION,
    mobxDevTools: MOBX_DEV_TOOLS,
    current: CURRENT_NODE_ENV,
    isDev,
    isTest,
    isProduction,
    isMainnet,
    isStaging,
    isTestnet,
    isAlonzoPurple,
    isShelleyQA,
    isSelfnode,
    isDevelopment,
    isWatchMode,
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
  },
  process.env
);
