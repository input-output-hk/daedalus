// WatchdogState — snapshot of watchdog state, returned by poll channel
export interface WatchdogState {
  watchdogPid: number;
  nodePid: number;
  walletPid: number;
  nodeStartedAt: number | null;
  walletStartedAt: number | null;
  walletRestartCount: number;
  walletPort: number | null;
  hasChain: boolean | null;
  nodeStartupPhase: string | null;
  blockSyncProgress: { replayedBlock: number; validatingChunk: number; pushingLedger: number; };
  mithrilPhase: string | null;
  mithrilProgress: MithrilProgress | null;
  lastError: string | null;
  walletUnrecoverable: boolean;
  nodeSocketWaitMs: number | null;
  walletReadyWaitMs: number | null;
  nodeForceKilled: boolean;
  lastWalletExitCode: number | null;
  lastWalletExitSignal: string | null;
  // Chain storage paths (Daedalus config, not watchdog state)
  defaultChainPath: string | null;
  customChainPath: string | null;
}

// MithrilProgress — from mithril_progress events
export interface MithrilProgress {
  filesDownloaded: number;
  filesTotal: number;
  bytesDownloaded: number;
  bytesTotal: number;
  secondsElapsed: number;
  stepNum: number;
  totalSteps: number;
  phase: 'snapshot' | 'ledger';
}

// LoadingPhase — what the renderer loading screen uses
export type LoadingPhase =
  | 'starting'
  | 'chain-storage-setup'
  | 'bootstrap-decision'
  | 'mithril-syncing'
  | 'node-starting'
  | 'ready'
  | 'error';

// MithrilCommand — renderer→main commands forwarded to watchdog
export type MithrilCommand =
  | { cmd: 'start_mithril' }
  | { cmd: 'start_mithril'; force: true }
  | { cmd: 'start_mithril'; wipe_chain: true }
  | { cmd: 'start_node' }
  | { cmd: 'cancel_mithril' }
  | { cmd: 'probe_mithril' };

// MithrilPhase — string discriminant from mithril_status events
export type MithrilPhase =
  | 'preparing'
  | 'downloading'
  | 'verifying'
  | 'converting'
  | 'installing'
  | 'finalizing'
  | 'completed'
  | 'cancelled'
  | 'error';

// Compatibility aliases — existing loading-screen components import these.
// They map the old state-machine constants to LoadingPhase strings so the
// components work without structural changes.
export type CardanoNodeState = string;

export const CardanoNodeStates = {
  STARTING: 'node-starting',
  RUNNING: 'node-starting',
  EXITING: 'node-starting',
  STOPPING: 'node-starting',
  STOPPED: 'error',
  UPDATING: 'node-starting',
  UPDATED: 'node-starting',
  CRASHED: 'error',
  ERRORED: 'error',
  UNRECOVERABLE: 'error',
} as const;

// ChainStorageValidation — result of validating a user-chosen chain storage path
export type ChainStorageValidationReason =
  | 'path-not-found'
  | 'not-writable'
  | 'inside-state-dir'
  | 'is-managed-child'
  | 'insufficient-space'
  | 'path-is-file'
  | 'unknown';

export interface ChainStorageValidation {
  isValid: boolean;
  path?: string | null;
  resolvedPath?: string;
  reason?: ChainStorageValidationReason;
  availableSpaceBytes?: number;
  requiredSpaceBytes?: number;
  chainSubdirectoryStatus?: 'existing-directory' | 'will-create';
}

// Compat aliases for Mithril component imports that referenced deleted type files.
export type MithrilBootstrapStatus = string;
export type MithrilPartialSyncStatus = string;
export type MithrilSyncStatus = string;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type MithrilProgressItem = any;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type MithrilBootstrapError = any;
export type MithrilBootstrapErrorStage = string;
export interface MithrilSnapshotItem {
  digest: string;
  createdAt: string;
  size: number;
  [key: string]: unknown;
}
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type MithrilBootstrapStatusUpdate = any;
export const isMithrilPartialSyncRestoreCompleteStatus = (_s: string): boolean => false;
export const isMithrilBootstrapRestoreCompleteStatus = (_s: string): boolean => false;
export const isMithrilSyncRestoreCompleteStatus = (_s: string): boolean => false;

// Partial sync error types (used by partialSyncErrorCopy.ts)
export type MithrilPartialSyncErrorCode =
  | 'PARTIAL_SYNC_LATEST_DRIFT'
  | 'PARTIAL_SYNC_STAGED_DB_INVALID'
  | 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'
  | 'PARTIAL_SYNC_CONVERSION_FAILED'
  | 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE'
  | 'PARTIAL_SYNC_METADATA_UNAVAILABLE'
  | 'PARTIAL_SYNC_DISABLED'
  | 'PARTIAL_SYNC_ALREADY_RUNNING'
  | 'PARTIAL_SYNC_START_NOT_ALLOWED'
  | 'PARTIAL_SYNC_LAYOUT_UNSUPPORTED'
  | 'PARTIAL_SYNC_CANCEL_NOT_ALLOWED'
  | 'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'
  | 'PARTIAL_SYNC_MANAGED_CHAIN_INVALID'
  | 'PARTIAL_SYNC_IMMUTABLE_INVALID'
  | 'PARTIAL_SYNC_PROTOCOL_MAGIC_INVALID'
  | 'PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE'
  | 'PARTIAL_SYNC_STAGING_INSIDE_MANAGED_CHAIN';

export type MithrilPartialSyncErrorStage =
  | 'downloading'
  | 'verifying'
  | 'converting'
  | 'installing'
  | 'preparing'
  | 'finalizing'
  | 'starting-node'
  | 'stopping-node';

export interface MithrilPartialSyncError {
  code?: MithrilPartialSyncErrorCode;
  stage?: MithrilPartialSyncErrorStage;
  message?: string;
  logPath?: string;
}
