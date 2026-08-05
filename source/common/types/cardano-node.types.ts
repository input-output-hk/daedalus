import {
  MAINNET,
  TESTNET,
  STAGING,
  SHELLEY_QA,
  ALONZO_PURPLE,
  VASIL_DEV,
  PREPROD,
  PREVIEW,
  SELFNODE,
} from './environment.types';

export type NetworkNames =
  | 'mainnet'
  | 'testnet'
  | 'staging'
  | 'shelley_qa'
  | 'alonzo_purple'
  | 'vasil_dev'
  | 'preprod'
  | 'preview'
  | 'selfnode'
  | 'development'
  | string;
export type PlatformNames = 'win32' | 'linux' | 'darwin' | string;
export const NetworkNameOptions = {
  mainnet: 'mainnet',
  testnet: 'testnet',
  staging: 'staging',
  shelley_qa: 'shelley_qa',
  alonzo_purple: 'alonzo_purple',
  vasil_dev: 'vasil_dev',
  preprod: 'preprod',
  preview: 'preview',
  selfnode: 'selfnode',
  development: 'development',
};

export type NetworkMagicType = Array<number | null | undefined>;
export const NetworkMagics: {
  mainnet: NetworkMagicType;
  testnet: NetworkMagicType;
  staging: NetworkMagicType;
  alonzo_purple: NetworkMagicType;
  vasil_dev: NetworkMagicType;
  preprod: NetworkMagicType;
  preview: NetworkMagicType;
  shelley_qa: NetworkMagicType;
  selfnode: NetworkMagicType;
} = {
  [MAINNET]: [1, null],
  [STAGING]: [633343913, 1],
  [TESTNET]: [1097911063, 0],
  [ALONZO_PURPLE]: [8, 0],
  [VASIL_DEV]: [9, 0],
  [PREPROD]: [1, 0],
  [PREVIEW]: [2, 0],
  [SHELLEY_QA]: [3, 0],
  [SELFNODE]: [1, null],
};

export enum BlockSyncType {
  pushingLedger = 'pushingLedger',
  replayedBlock = 'replayedBlock',
  validatingChunk = 'validatingChunk',
}

export type NodeStartupPhase =
  | 'openingChainDb'
  | 'openingImmutableDb'
  | 'openedImmutableDb'
  | 'openingVolatileDb'
  | 'openedVolatileDb'
  | 'openingLedgerDb'
  | 'replayingLedger'
  | 'openedLedgerDb'
  | 'chainDbReady';
