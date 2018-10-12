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
  'updated' | 'crashed' | 'errored' | 'exiting'
);

export const CardanoNodeStates: {
  STARTING: CardanoNodeState,
  RUNNING: CardanoNodeState;
  EXITING: CardanoNodeState;
  STOPPING: CardanoNodeState;
  STOPPED: CardanoNodeState;
  UPDATING: CardanoNodeState;
  UPDATED: CardanoNodeState;
  CRASHED: CardanoNodeState;
  ERRORED: CardanoNodeState;
} = {
  STARTING: 'starting',
  RUNNING: 'running',
  EXITING: 'exiting',
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

export type CardanoNodeProcessNames = (
  'cardano-node' | 'cardano-node.exe'
);

export type ProcessNames = {
  CARDANO_PROCESS_NAME: CardanoNodeProcessNames
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
