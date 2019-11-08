// @flow
export type Environment = {
  network: string,
  apiVersion: string,
  mobxDevTools: boolean | string,
  current: string,
  isDev: boolean,
  isTest: boolean,
  isProduction: boolean,
  isMainnet: boolean,
  isStaging: boolean,
  isTestnet: boolean,
  isItnBalanceCheck: boolean,
  isDevelopment: boolean,
  isWatchMode: boolean,
  build: string,
  buildNumber: string,
  buildLabel: string,
  platform: string,
  platformVersion: string,
  mainProcessID: string,
  rendererProcessID: string,
  os: string,
  cpu: string,
  ram: number,
  installerVersion: string,
  version: string,
  isWindows: boolean,
  isMacOS: boolean,
  isLinux: boolean,
  isBlankScreenFixActive: boolean,
};

// constants
export const PRODUCTION = 'production';
export const DEVELOPMENT = 'development';
export const TEST = 'test';
export const MAINNET = 'mainnet';
export const NIGHTLY = 'nightly';
export const ITN_BALANCE_CHECK = 'itn_balance_check';
export const QA = 'qa';
export const SELFNODE = 'selfnode';
export const STAGING = 'staging';
export const TESTNET = 'testnet';
export const MAC_OS = 'darwin';
export const WINDOWS = 'win32';
export const LINUX = 'linux';
export const OS_NAMES = {
  [MAC_OS]: 'macOS',
  [WINDOWS]: 'Windows',
  [LINUX]: 'Linux',
};

export const networkPrettyNames = {
  production: 'Production',
  development: 'Production',
  itn_balance_check: 'Incentivized Testnet - Balance check',
};
