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

export type PlatformNames = (
  'win32' | 'linux' | 'darwin' | string
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

export type CardanoNodeStorageKeys = {
  PREVIOUS_CARDANO_PID: CardanoPidOptions
};

// TODO: Add more options when the exact process names are established
export type DaedalusProcessNames = (
  'Electron'
);

// TODO: Determine if the cardano-node can be named anything else
export type CardanoNodeProcessNames = (
  'cardano-node' | 'cardano-node.exe'
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

export const CardanoProcessNameOptions: {
  win32: CardanoNodeProcessNames,
  linux: CardanoNodeProcessNames,
  darwin: CardanoNodeProcessNames,
} = {
  win32: 'cardano-node.exe',
  linux: 'cardano-node',
  darwin: 'cardano-node'
};
