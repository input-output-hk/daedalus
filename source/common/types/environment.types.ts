export type Environment = {
  network: Network;
  apiVersion: string;
  nodeVersion: string;
  mobxDevTools: boolean | string;
  current: string;
  isDev: boolean;
  isTest: boolean;
  isProduction: boolean;
  isMainnet: boolean;
  isStaging: boolean;
  isTestnet: boolean;
  isAlonzoPurple: boolean;
  isShelleyQA: boolean;
  isSelfnode: boolean;
  isDevelopment: boolean;
  isWatchMode: boolean;
  build: string;
  buildNumber: string;
  platform: string;
  platformVersion: string;
  mainProcessID: string;
  rendererProcessID: string;
  os: string;
  cpu: string;
  ram: number;
  installerVersion: string;
  version: string;
  isWindows: boolean;
  isMacOS: boolean;
  isLinux: boolean;
  isBlankScreenFixActive: boolean;
  keepLocalClusterRunning: boolean;
};
// constants
export const PRODUCTION = 'production';
export const DEVELOPMENT = 'development';
export const TEST = 'test';
// cardano-node networks
export const MAINNET = 'mainnet';
export const MAINNET_FLIGHT = 'mainnet_flight';
export const TESTNET = 'testnet';
export const STAGING = 'staging';
export const SHELLEY_QA = 'shelley_qa';
export const ALONZO_PURPLE = 'alonzo_purple';
export const SELFNODE = 'selfnode';
export const MAC_OS = 'darwin';
export const WINDOWS = 'win32';
export const LINUX = 'linux';
export const OS_NAMES = {
  [MAC_OS]: 'macOS',
  [WINDOWS]: 'Windows',
  [LINUX]: 'Linux',
};
export type Platform = 'darwin' | 'win32' | 'linux';
export type Network =
  | 'mainnet'
  | 'mainnet_flight'
  | 'testnet'
  | 'staging'
  | 'shelley_qa'
  | 'alonzo_purple'
  | 'selfnode'
  | 'development';
export const networkPrettyNames = {
  mainnet: 'Mainnet',
  testnet: 'Testnet',
  staging: 'Staging',
  shelley_qa: 'Shelley QA',
  alonzo_purple: 'Alonzo Purple',
  selfnode: 'Selfnode',
  development: 'Development',
};
