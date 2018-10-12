// @flow
import os from 'os';
import { uniq } from 'lodash';
import { version } from '../../package.json';
import type { Environment } from './types/environment.types';

// Only require electron / remote if we are in a node.js environment
let remote;
if (process.version !== '' && process.release !== undefined && process.release.name === 'node') {
  remote = require('electron').remote;
}

// constants
const PRODUCTION = 'production';
const DEVELOPMENT = 'development';
const TEST = 'test';
const MAINNET = 'mainnet';
const STAGING = 'staging';
const TESTNET = 'testnet';
const MAC_OS = 'darwin';
const WINDOWS = 'win32';
const LINUX = 'linux';
const OS_NAMES = {
  [MAC_OS]: 'macOS',
  [WINDOWS]: 'Windows',
  [LINUX]: 'Linux',
};

// environment variables
const STAGING_REPORT_URL = 'http://staging-report-server.awstest.iohkdev.io:8080/';
const CURRENT_NODE_ENV = process.env.NODE_ENV || DEVELOPMENT;
const NETWORK = process.env.NETWORK || DEVELOPMENT;
const REPORT_URL = process.env.REPORT_URL || STAGING_REPORT_URL;
const isDev = CURRENT_NODE_ENV === DEVELOPMENT;
const isTest = CURRENT_NODE_ENV === TEST;
const isProduction = CURRENT_NODE_ENV === PRODUCTION;
const isMainnet = CURRENT_NODE_ENV === MAINNET;
const isStaging = CURRENT_NODE_ENV === STAGING;
const isTestnet = CURRENT_NODE_ENV === TESTNET;
const API_VERSION = process.env.API_VERSION || 'dev';
const PLATFORM = os.platform();
const OS = OS_NAMES[PLATFORM] || PLATFORM;
const BUILD = process.env.BUILD_NUMBER || 'dev';
const BUILD_NUMBER = uniq([API_VERSION, BUILD]).join('.');
const BUILD_LABEL = (isProduction ?
  `Daedalus (${version}#${BUILD_NUMBER})` :
  `Daedalus (${version}#${BUILD_NUMBER}) ${CURRENT_NODE_ENV}`
);
const INSTALLER_VERSION = uniq([API_VERSION, BUILD]).join('.');
const MOBX_DEV_TOOLS = process.env.MOBX_DEV_TOOLS;
const isMacOS = PLATFORM === MAC_OS;
const isWindows = PLATFORM === WINDOWS;
const isLinux = PLATFORM === LINUX;

// compose environment
const daedalusEnv = Object.assign({}, {
  DEVELOPMENT,
  TEST,
  PRODUCTION,
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
});
const sourceEnv = remote ? remote.getGlobal('env') : process.env;

export const environment: Environment = Object.assign(daedalusEnv, sourceEnv);
