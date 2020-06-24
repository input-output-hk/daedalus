// @flow
export type Environment = {
  network: Network,
  rawNetwork: string,
  apiVersion: string,
  nodeVersion: string,
  mobxDevTools: boolean | string,
  current: string,
  isDev: boolean,
  isTest: boolean,
  isProduction: boolean,
  isMainnet: boolean,
  isStaging: boolean,
  isTestnet: boolean,
  isSelfnode: boolean,
  isIncentivizedTestnet: boolean,
  isIncentivizedTestnetQA: boolean,
  isIncentivizedTestnetNightly: boolean,
  isIncentivizedTestnetSelfnode: boolean,
  isDevelopment: boolean,
  isWatchMode: boolean,
  build: string,
  buildNumber: string,
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

// cardano-node networks
export const MAINNET = 'mainnet';
export const MAINNET_FLIGHT = 'mainnet_flight';
export const SELFNODE = 'selfnode';
export const STAGING = 'staging';
export const TESTNET = 'testnet';
export const SHELLEY_TESTNET = 'shelley_testnet';
export const SHELLEY_QA = 'shelley_qa';

// jormungandr networks
export const ITN_REWARDS_V1 = 'itn_rewards_v1';
export const ITN_SELFNODE = 'itn_selfnode';
export const QA = 'qa';
export const NIGHTLY = 'nightly';

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
  | 'mainnet_flight'
  | 'selfnode'
  | 'staging'
  | 'testnet'
  | 'shelley_testnet'
  | 'shelley_qa'
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
  selfnode: 'Selfnode',
  staging: 'Staging',
  testnet: 'Testnet',
  shelley_testnet: 'Shelley Testnet',
  shelley_qa: 'Shelley QA',
  development: 'Development',
  itn_rewards_v1: 'Incentivized Testnet v1 - Rewards',
};
