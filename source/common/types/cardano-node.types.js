// @flow
export type TlsConfig = {
  hostname: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};

export type CardanoNodeImplementation = 'jormungandr' | 'cardano-node';

export type NetworkNames =
  | 'mainnet'
  | 'staging'
  | 'testnet'
  | 'development'
  | 'itn_rewards_v1'
  | string;

export type PlatformNames = 'win32' | 'linux' | 'darwin' | string;

export const NetworkNameOptions = {
  mainnet: 'mainnet',
  staging: 'staging',
  testnet: 'testnet',
  development: 'development',
  itn_rewards_v1: 'itn_rewards_v1',
};

export type CardanoNodeState =
  | 'starting'
  | 'running'
  | 'exiting'
  | 'stopping'
  | 'stopped'
  | 'updating'
  | 'updated'
  | 'crashed'
  | 'errored'
  | 'unrecoverable';

export const CardanoNodeStates: EnumMap<string, CardanoNodeState> = {
  STARTING: 'starting',
  RUNNING: 'running',
  EXITING: 'exiting',
  STOPPING: 'stopping',
  STOPPED: 'stopped',
  UPDATING: 'updating',
  UPDATED: 'updated',
  CRASHED: 'crashed',
  ERRORED: 'errored',
  UNRECOVERABLE: 'unrecoverable',
};

export type CardanoPidOptions =
  | 'mainnet-PREVIOUS-CARDANO-PID'
  | 'staging-PREVIOUS-CARDANO-PID'
  | 'testnet-PREVIOUS-CARDANO-PID'
  | 'development-PREVIOUS-CARDANO-PID'
  | 'itn_rewards_v1-PREVIOUS-CARDANO-PID'
  | string;

export type CardanoNodeStorageKeys = {
  PREVIOUS_CARDANO_PID: CardanoPidOptions,
};

export type CardanoNodeProcessNames =
  | 'cardano-wallet-jormungandr'
  | 'cardano-wallet-jormungandr.exe';

export type ProcessNames = {
  CARDANO_PROCESS_NAME: CardanoNodeProcessNames,
};

export const CardanoProcessNameOptions: {
  win32: CardanoNodeProcessNames,
  linux: CardanoNodeProcessNames,
  darwin: CardanoNodeProcessNames,
} = {
  win32: 'cardano-wallet-jormungandr.exe',
  linux: 'cardano-wallet-jormungandr',
  darwin: 'cardano-wallet-jormungandr',
};

/**
 * Expected fault injection types that can be used to tell
 * cardano-node to behave faulty (useful for testing)
 */
export type FaultInjection =
  | 'FInjIgnoreShutdown'
  | 'FInjIgnoreAPI'
  | 'FInjApplyUpdateNoExit'
  | 'FInjApplyUpdateWrongExitCode';

export const FaultInjections: {
  IgnoreShutdown: FaultInjection,
  IgnoreApi: FaultInjection,
  ApplyUpdateNoExit: FaultInjection,
  ApplyUpdateWrongExitCode: FaultInjection,
} = {
  IgnoreShutdown: 'FInjIgnoreShutdown',
  IgnoreApi: 'FInjIgnoreAPI',
  ApplyUpdateNoExit: 'FInjApplyUpdateNoExit',
  ApplyUpdateWrongExitCode: 'FInjApplyUpdateWrongExitCode',
};

export type FaultInjectionIpcResponse = Array<FaultInjection>;
export type FaultInjectionIpcRequest = [FaultInjection, boolean];

export type CardanoStatus = {
  isNodeResponding: boolean,
  isNodeSyncing: boolean,
  isNodeInSync: boolean,
  hasBeenConnected: boolean,
  cardanoNodeID: number,
};
