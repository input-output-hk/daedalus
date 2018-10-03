// @flow
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

export type TlsConfig = {
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};
