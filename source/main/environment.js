// @flow
import os from 'os';
import { uniq, upperFirst, get, includes } from 'lodash';
import { version } from '../../package.json';
import type { Environment } from '../common/types/environment.types';
import {
  DEVELOPMENT,
  LINUX,
  MAC_OS,
  MAINNET,
  OS_NAMES,
  PRODUCTION,
  STAGING,
  TEST,
  TESTNET,
  WINDOWS,
} from '../common/types/environment.types';

// environment variables
const CURRENT_NODE_ENV = process.env.NODE_ENV || DEVELOPMENT;
const NETWORK = process.env.NETWORK || DEVELOPMENT;
const isDev = CURRENT_NODE_ENV === DEVELOPMENT;
const isTest = CURRENT_NODE_ENV === TEST;
const isProduction = CURRENT_NODE_ENV === PRODUCTION;
const isMainnet = NETWORK === MAINNET;
const isStaging = NETWORK === STAGING;
const isTestnet = NETWORK === TESTNET;
const isDevelopment = NETWORK === DEVELOPMENT;
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
const BUILD_LABEL = (() => {
  const networkLabel = !(isMainnet || isDev) ? ` ${upperFirst(NETWORK)}` : '';
  let buildLabel = `Daedalus AR${networkLabel} (${version}#${BUILD_NUMBER})`;
  if (!isProduction) buildLabel += ` ${CURRENT_NODE_ENV}`;
  return buildLabel;
})();
const INSTALLER_VERSION = uniq([API_VERSION, BUILD]).join('.');
const MOBX_DEV_TOOLS = process.env.MOBX_DEV_TOOLS || false;
const isMacOS = PLATFORM === MAC_OS;
const isWindows = PLATFORM === WINDOWS;
const isLinux = PLATFORM === LINUX;

// compose environment
export const environment: Environment = Object.assign(
  {},
  {
    network: NETWORK,
    apiVersion: API_VERSION,
    mobxDevTools: MOBX_DEV_TOOLS,
    current: CURRENT_NODE_ENV,
    isDev,
    isTest,
    isProduction,
    isMainnet,
    isStaging,
    isTestnet,
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
