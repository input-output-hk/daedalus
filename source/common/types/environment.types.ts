export type Environment = {
  network: Network;
  apiVersion: string;
  nodeVersion: string;
  current: string;
  isDev: boolean;
  isTest: boolean;
  isProduction: boolean;
  isMainnet: boolean;
  isStaging: boolean;
  isTestnet: boolean;
  isAlonzoPurple: boolean;
  isVasilDev: boolean;
  isPreprod: boolean;
  isPreview: boolean;
  isShelleyQA: boolean;
  isSelfnode: boolean;
  isDevelopment: boolean;
  build: string;
  buildNumber: string;
  platform: string;
  platformVersion: string;
  mainProcessID: string;
  rendererProcessID: string;
  os: string;
  system: string;
  cpu: Cpu;
  ram: number;
  hasMetHardwareRequirements: boolean;
  installerVersion: string;
  version: string;
  isWindows: boolean;
  isMacOS: boolean;
  isLinux: boolean;
  isBlankScreenFixActive: boolean;
  keepLocalClusterRunning: boolean;
  analyticsFeatureEnabled: boolean;
  catalystApiUrlOverride?: string;
  votingVisibleOverride: boolean;
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
export const VASIL_DEV = 'vasil_dev';
export const PREPROD = 'preprod';
export const PREVIEW = 'preview';
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
  | 'vasil_dev'
  | 'preprod'
  | 'preview'
  | 'selfnode'
  | 'development';
export const networkPrettyNames = {
  mainnet: 'Mainnet',
  testnet: 'Testnet',
  staging: 'Staging',
  shelley_qa: 'Shelley QA',
  alonzo_purple: 'Alonzo Purple',
  vasil_dev: 'Vasil-Dev',
  preprod: 'Pre-Prod',
  preview: 'Preview',
  selfnode: 'Selfnode',
  development: 'Development',
};
export type CpuThreadData = {
  model: string;
  speed: number;
  times: Record<string, number>;
};
export type Cpu = Array<CpuThreadData>;
