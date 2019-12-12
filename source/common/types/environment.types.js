// @flow
export type Environment = {
  network: Network,
  rawNetwork: string,
  apiVersion: string,
  mobxDevTools: boolean | string,
  current: string,
  isDev: boolean,
  isTest: boolean,
  isProduction: boolean,
  isMainnet: boolean,
  isStaging: boolean,
  isTestnet: boolean,
  isIncentivizedTestnet: boolean,
  isIncentivizedTestnetQA: boolean,
  isIncentivizedTestnetNightly: boolean,
  isIncentivizedTestnetSelfNode: boolean,
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
export const ITN_REWARDS_V1 = 'itn_rewards_v1';
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

export type Network =
  | 'mainnet'
  | 'staging'
  | 'testnet'
  | 'development'
  | 'itn'
  | 'itn_rewards_v1'
  | 'itn_rewards_v1_selfnode'
  | 'itn_rewards_v1_qa'
  | 'itn_rewards_v1_nightly'
  | 'itn_rewards'
  | 'itn_rewards_selfnode'
  | 'itn_rewards_qa'
  | 'itn_rewards_nightly';

export const networkPrettyNames = {
  mainnet: 'Mainnet',
  staging: 'Staging',
  testnet: 'Testnet',
  development: 'Development',
  itn_rewards_v1: 'Incentivized Testnet v1 - Rewards',
};
