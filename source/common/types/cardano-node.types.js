// @flow
export type TlsConfig = {
  hostname: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};

export type CardanoNodeImplementations = 'cardano' | 'selfnode';

export const CardanoNodeImplementationOptions: {
  CARDANO: CardanoNodeImplementations,
  SELFNODE: CardanoNodeImplementations,
} = {
  CARDANO: 'cardano',
  SELFNODE: 'selfnode',
};

export type NetworkNames =
  | 'mainnet'
  | 'testnet'
  | 'staging'
  | 'shelley_qa'
  | 'selfnode'
  | 'development'
  | string;

export type PlatformNames = 'win32' | 'linux' | 'darwin' | string;

export const NetworkNameOptions = {
  mainnet: 'mainnet',
  testnet: 'testnet',
  staging: 'staging',
  shelley_qa: 'shelley_qa',
  selfnode: 'selfnode',
  development: 'development',
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
  | 'testnet-PREVIOUS-CARDANO-PID'
  | 'staging-PREVIOUS-CARDANO-PID'
  | 'shelley_qa-PREVIOUS-CARDANO-PID'
  | 'selfnode-PREVIOUS-CARDANO-PID'
  | 'development-PREVIOUS-CARDANO-PID'
  | string;

export type CardanoNodeStorageKeys = {
  PREVIOUS_CARDANO_PID: CardanoPidOptions,
};

export type CardanoNodeProcessNames =
  | 'cardano-node'
  | 'cardano-node.exe'
  | 'local-cluster'
  | 'local-cluster.exe';

export type ProcessNames = {
  CARDANO_PROCESS_NAME: CardanoNodeProcessNames,
};

export const CardanoProcessNameOptions: {
  [CardanoNodeImplementations]: {
    win32: CardanoNodeProcessNames,
    linux: CardanoNodeProcessNames,
    darwin: CardanoNodeProcessNames,
  },
} = {
  cardano: {
    win32: 'cardano-node.exe',
    linux: 'cardano-node',
    darwin: 'cardano-node',
  },
  selfnode: {
    win32: 'local-cluster.exe',
    linux: 'local-cluster',
    darwin: 'local-cluster',
  },
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
  cardanoNodePID: number,
  cardanoWalletPID: number,
};

// Cardano Mainet network magic
export const MAINNET_MAGIC = [1, null];

// Cardano Byron Testnet network magic
export const TESTNET_MAGIC = [1097911063, 0];

// Cardano Staging network magic
export const STAGING_MAGIC = [633343913, 1];

// Cardano Selfnode network magic
export const SELFNODE_MAGIC = MAINNET_MAGIC;
