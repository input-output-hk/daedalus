// Re-export shared types from bootstrap types (don't duplicate)
export type {
  MithrilProgressItem,
  MithrilProgressItemState,
  MithrilSnapshotItem,
} from './mithril-bootstrap.types';
export type {
  ChainStorageConfig,
  ChainSubdirectoryStatus,
  ChainStorageValidation,
} from './mithril-bootstrap.types';

export type MithrilSyncFlowType = 'bootstrap' | 'partial-sync';

// Union of all statuses from both flows
export type MithrilSyncStatus =
  | 'idle'
  | 'decision'
  | 'stopping-node'
  | 'cancelling'
  | 'preparing'
  | 'downloading'
  | 'verifying'
  | 'unpacking'
  | 'installing'
  | 'converting'
  | 'finalizing'
  | 'starting-node'
  | 'completed'
  | 'failed'
  | 'cancelled';

export type MithrilPartialSyncFailureAction =
  | 'retry'
  | 'restart-normal'
  | 'wipe-and-full-sync';

export type MithrilSyncErrorCode = string; // keep loose - partial sync has many specific codes

export type MithrilSyncError = {
  message: string;
  code?: MithrilSyncErrorCode;
  logPath?: string;
  stage?: string;
};

export type MithrilSyncStatusUpdate = {
  status: MithrilSyncStatus;
  flowType: MithrilSyncFlowType;
  // Progress (both flows)
  filesDownloaded?: number;
  filesTotal?: number;
  snapshotBytesDownloaded?: number;
  snapshotBytesTotal?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  progressItems?: import('./mithril-bootstrap.types').MithrilProgressItem[];
  error?: MithrilSyncError | null;
  logPath?: string;
  // Bootstrap-specific
  snapshot?: import('./mithril-bootstrap.types').MithrilSnapshotItem | null;
  // Partial-sync-specific
  allowedRecoveryActions?: MithrilPartialSyncFailureAction[];
};

// Status predicates
export const MITHRIL_SYNC_WORKING_STATUSES: ReadonlyArray<MithrilSyncStatus> = [
  'stopping-node',
  'cancelling',
  'preparing',
  'downloading',
  'verifying',
  'unpacking',
  'installing',
  'converting',
  'finalizing',
  'starting-node',
];

export const MITHRIL_SYNC_TERMINAL_STATUSES: ReadonlyArray<MithrilSyncStatus> =
  ['completed', 'failed', 'cancelled'];

export const isMithrilSyncWorkingStatus = (s: MithrilSyncStatus): boolean =>
  MITHRIL_SYNC_WORKING_STATUSES.includes(s);

export const isMithrilSyncTerminalStatus = (s: MithrilSyncStatus): boolean =>
  MITHRIL_SYNC_TERMINAL_STATUSES.includes(s);

export const isMithrilSyncRestoreCompleteStatus = (
  s: MithrilSyncStatus
): boolean => s === 'completed' || s === 'starting-node';

export const isMithrilSyncOverlayStatus = (s: MithrilSyncStatus): boolean =>
  isMithrilSyncWorkingStatus(s) || isMithrilSyncTerminalStatus(s);

export const isMithrilSyncBlockingNodeStart = (s: MithrilSyncStatus): boolean =>
  [
    'stopping-node',
    'cancelling',
    'preparing',
    'downloading',
    'verifying',
    'unpacking',
    'installing',
    'converting',
    'finalizing',
    'starting-node',
  ].includes(s);

export const isMithrilSyncSuppressingDiskSpaceCheck = (
  s: MithrilSyncStatus
): boolean =>
  [
    'preparing',
    'downloading',
    'verifying',
    'unpacking',
    'installing',
    'converting',
    'finalizing',
    'starting-node',
  ].includes(s);
