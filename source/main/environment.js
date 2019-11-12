// @flow
import os from 'os';
import { uniq, get, includes } from 'lodash';
import { version } from '../../package.json';
import type { Environment } from '../common/types/environment.types';
import { DEVELOPMENT, OS_NAMES } from '../common/types/environment.types';
import {
  evaluateNetwork,
  checkIsDev,
  checkIsTest,
  checkIsProduction,
  checkIsMainnet,
  checkIsStaging,
  checkIsTestnet,
  checkIsDevelopment,
  checkIsIncentivizedTestnet,
  checkIsIncentivizedTestnetQA,
  checkIsIncentivizedTestnetNightly,
  getBuildLabel,
  checkIsMacOS,
  checkIsWindows,
  checkIsLinux,
} from '../common/utils/environmentCheckers';

/* ==================================================================
=                           Evaluations                             =
================================================================== */

// environment variables
const CURRENT_NODE_ENV = process.env.NODE_ENV || DEVELOPMENT;
const RAW_NETWORK = process.env.NETWORK || '';
const NETWORK = evaluateNetwork(process.env.NETWORK);
const isDev = checkIsDev(CURRENT_NODE_ENV);
const isTest = checkIsTest(CURRENT_NODE_ENV);
const isProduction = checkIsProduction(CURRENT_NODE_ENV);
const isMainnet = checkIsMainnet(NETWORK);
const isStaging = checkIsStaging(NETWORK);
const isTestnet = checkIsTestnet(NETWORK);
const isIncentivizedTestnet = checkIsIncentivizedTestnet(NETWORK);
const isIncentivizedTestnetQA = checkIsIncentivizedTestnetQA(RAW_NETWORK);
const isIncentivizedTestnetNightly = checkIsIncentivizedTestnetNightly(
  RAW_NETWORK
);
const isDevelopment = checkIsDevelopment(NETWORK);
const isWatchMode = process.env.IS_WATCH_MODE;
const API_VERSION = process.env.API_VERSION || 'dev';
const mainProcessID = get(process, 'ppid', '-');
const rendererProcessID = process.pid;
const PLATFORM = os.platform();
const PLATFORM_VERSION = os.release();
const OS = OS_NAMES[PLATFORM] || PLATFORM;
const cpu = os.cpus();
const ram = os.totalmem();
const isBlankScreenFixActive = includes(process.argv.slice(1), '--safe-mode');
const BUILD = process.env.BUILD_NUMBER || 'dev';
const BUILD_NUMBER = uniq([API_VERSION, BUILD]).join('.');
const BUILD_LABEL = getBuildLabel(
  BUILD_NUMBER,
  NETWORK,
  CURRENT_NODE_ENV,
  version
);
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
    rawNetwork: RAW_NETWORK,
    apiVersion: API_VERSION,
    mobxDevTools: MOBX_DEV_TOOLS,
    current: CURRENT_NODE_ENV,
    isDev,
    isTest,
    isProduction,
    isMainnet,
    isStaging,
    isTestnet,
    isIncentivizedTestnet,
    isIncentivizedTestnetQA,
    isIncentivizedTestnetNightly,
    isDevelopment,
    isWatchMode,
    build: BUILD,
    buildNumber: BUILD_NUMBER,
    buildLabel: BUILD_LABEL,
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
  },
  process.env
);
