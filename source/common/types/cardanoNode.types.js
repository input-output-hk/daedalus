// @flow
export type TlsConfig = {
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};

export type NetworkNames = (
  'mainnet' | 'staging' | 'testnet' | 'development' | string
);

export const NetworkNameOptions = {
  mainnet: 'mainnet',
  staging: 'staging',
  testnet: 'testnet',
  development: 'development'
};

export type CardanoNodeState = (
  'stopped' | 'starting' | 'running' | 'stopping' | 'updating' |
  'updated' | 'crashed' | 'errored'
);

export const CardanoNodeStates: {
  STARTING: CardanoNodeState,
  RUNNING: CardanoNodeState;
  STOPPING: CardanoNodeState;
  STOPPED: CardanoNodeState;
  UPDATING: CardanoNodeState;
  UPDATED: CardanoNodeState;
  CRASHED: CardanoNodeState;
  ERRORED: CardanoNodeState;
} = {
  STARTING: 'starting',
  RUNNING: 'running',
  STOPPING: 'stopping',
  STOPPED: 'stopped',
  UPDATING: 'updating',
  UPDATED: 'updated',
  CRASHED: 'crashed',
  ERRORED: 'errored',
};

export type CardanoPidOptions = (
  'mainnet-PREVIOUS-CARDANO-PID' |
  'staging-PREVIOUS-CARDANO-PID' |
  'testnet-PREVIOUS-CARDANO-PID' |
  'development-PREVIOUS-CARDANO-PID' |
  string
);

export type DaedalusPidOptions = (
  'mainnet-PREVIOUS-DAEDALUS-PID' |
  'staging-PREVIOUS-DAEDALUS-PID' |
  'testnet-PREVIOUS-DAEDALUS-PID' |
  'development-PREVIOUS-DAEDALUS-PID' |
  string
);

export type CardanoNodeStorageKeys = {
  PREVIOUS_CARDANO_PID: CardanoPidOptions,
  PREVIOUS_DAEDALUS_PID: DaedalusPidOptions
};

// TODO: Add more options when the exact process names are established
export type DaedalusProcessNames = (
  'Electron'
);

// TODO: Determine if the cardano-node can be named anything else
export type CardanoNodeProcessNames = (
  'cardano-node'
);

export type ProcessNames = {
  DAEDALUS_PROCESS_NAME: DaedalusProcessNames,
  CARDANO_PROCESS_NAME: CardanoNodeProcessNames
};

// TODO: Modify values when the exact process names are established
export const DaedalusProcessNameOptions: {
  mainnet: DaedalusProcessNames,
  staging: DaedalusProcessNames,
  testnet: DaedalusProcessNames,
  development: DaedalusProcessNames
} = {
  mainnet: 'Electron',
  staging: 'Electron',
  testnet: 'Electron',
  development: 'Electron'
};

// TODO: Determine if the cardano-node can be named anything else
export const CardanoProcessNameOptions: {
  mainnet: CardanoNodeProcessNames,
  staging: CardanoNodeProcessNames,
  testnet: CardanoNodeProcessNames,
  development: CardanoNodeProcessNames
} = {
  mainnet: 'cardano-node',
  staging: 'cardano-node',
  testnet: 'cardano-node',
  development: 'cardano-node'
};
