// @flow
import { uniq, upperFirst } from 'lodash';
import { version } from '../../package.json';
import type { Environment } from './types/environment.types';
import { getOsPlatform } from './utils/getOsPlatform';

export const isNodeEnvironment = (
  process.version !== '' &&
  process.release !== undefined &&
  process.release.name === 'node'
);

// The process env is exposed to the renderer as global via preload script
const processEnv = isNodeEnvironment ? process.env : global.process.env;

// constants
export const PRODUCTION = 'production';
export const DEVELOPMENT = 'development';
export const TEST = 'test';
export const MAINNET = 'mainnet';
export const STAGING = 'staging';
export const TESTNET = 'testnet';
export const STAGING_REPORT_URL = 'http://staging-report-server.awstest.iohkdev.io:8080/';
export const MAC_OS = 'darwin';
export const WINDOWS = 'win32';
export const LINUX = 'linux';
export const OS_NAMES = {
  [MAC_OS]: 'macOS',
  [WINDOWS]: 'Windows',
  [LINUX]: 'Linux',
};

// environment variables
const CURRENT_NODE_ENV = processEnv.NODE_ENV || DEVELOPMENT;
const NETWORK = processEnv.NETWORK || DEVELOPMENT;
const REPORT_URL = processEnv.REPORT_URL || STAGING_REPORT_URL;
const isDev = CURRENT_NODE_ENV === DEVELOPMENT;
const isTest = CURRENT_NODE_ENV === TEST;
const isProduction = CURRENT_NODE_ENV === PRODUCTION;
const isMainnet = CURRENT_NODE_ENV === MAINNET;
const isStaging = CURRENT_NODE_ENV === STAGING;
const isTestnet = CURRENT_NODE_ENV === TESTNET;
const isWatchMode = processEnv.IS_WATCH_MODE;
const API_VERSION = processEnv.API_VERSION || 'dev';
const PLATFORM = getOsPlatform(isNodeEnvironment);
const OS = OS_NAMES[PLATFORM] || PLATFORM;
const BUILD = processEnv.BUILD_NUMBER || 'dev';
const BUILD_NUMBER = uniq([API_VERSION, BUILD]).join('.');
const BUILD_LABEL = (() => {
  const networkLabel = !(isMainnet || isDev) ? ` ${upperFirst(NETWORK)}` : '';
  let buildLabel = `Daedalus${networkLabel} (${version}#${BUILD_NUMBER})`;
  if (!isProduction) buildLabel += ` ${CURRENT_NODE_ENV}`;
  return buildLabel;
})();
const INSTALLER_VERSION = uniq([API_VERSION, BUILD]).join('.');
const MOBX_DEV_TOOLS = processEnv.MOBX_DEV_TOOLS || false;
const isMacOS = PLATFORM === MAC_OS;
const isWindows = PLATFORM === WINDOWS;
const isLinux = PLATFORM === LINUX;

// compose environment
export const environment: Environment = Object.assign({}, {
  NETWORK,
  API_VERSION,
  MOBX_DEV_TOOLS,
  current: CURRENT_NODE_ENV,
  REPORT_URL,
  isDev,
  isTest,
  isProduction,
  isMainnet,
  isStaging,
  isTestnet,
  isWatchMode,
  build: BUILD,
  buildNumber: BUILD_NUMBER,
  buildLabel: BUILD_LABEL,
  platform: PLATFORM,
  os: OS,
  installerVersion: INSTALLER_VERSION,
  version,
  isWindows,
  isMacOS,
  isLinux
}, processEnv);
