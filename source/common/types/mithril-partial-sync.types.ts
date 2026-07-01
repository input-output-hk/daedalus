import type { MithrilProgressItem } from './mithril-bootstrap.types';

export type MithrilPartialSyncStatus =
  | 'idle'
  | 'stopping-node'
  | 'cancelling'
  | 'preparing'
  | 'downloading'
  | 'verifying'
  | 'converting'
  | 'installing'
  | 'finalizing'
  | 'completed'
  | 'starting-node'
  | 'failed'
  | 'cancelled';

export type MithrilPartialSyncFailureAction =
  | 'retry'
  | 'restart-normal'
  | 'wipe-and-full-sync';

export type MithrilPartialSyncErrorStage =
  | 'stopping-node'
  | 'preparing'
  | 'downloading'
  | 'verifying'
  | 'converting'
  | 'installing'
  | 'finalizing'
  | 'starting-node';

export type MithrilPartialSyncErrorCode =
  | 'PARTIAL_SYNC_NO_CERTIFIED_RANGE'
  | 'PARTIAL_SYNC_LATEST_DRIFT'
  | 'PARTIAL_SYNC_STAGED_DB_INVALID'
  | 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'
  | 'PARTIAL_SYNC_CONVERSION_FAILED';

export type MithrilPartialSyncError = {
  message: string;
  code?: string;
  logPath?: string;
  stage?: MithrilPartialSyncErrorStage;
};

export type MithrilPartialSyncTransferProgress = {
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
};

export type MithrilPartialSyncStatusSnapshot = {
  status: MithrilPartialSyncStatus;
  allowedRecoveryActions: MithrilPartialSyncFailureAction[];
  transferProgress: MithrilPartialSyncTransferProgress;
  progressItems: MithrilProgressItem[];
  error: MithrilPartialSyncError | null;
  logPath?: string;
};

export type MithrilPartialSyncAvailability = {
  isEnabled: boolean;
  isSignificantlyBehind: boolean;
  behindByImmutables?: number;
  // #16 (D-702b-10): the Mithril certified-beacon epoch — the horizon-free,
  // early-resolving fallback anchor for cardano-wallet's late `networkTip.epoch`.
  // OPTIONAL so the renderer type-checks before CAT-H produces the value; until
  // then it is `undefined` ⇒ the figure degrades to networkTip-only (no regression).
  certifiedEpoch?: number | null;
};

const MITHRIL_PARTIAL_SYNC_WORKING_STATUSES: MithrilPartialSyncStatus[] = [
  'stopping-node',
  'cancelling',
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
];

const MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES: MithrilPartialSyncStatus[] = [
  'completed',
  'failed',
  'cancelled',
];

const MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES: MithrilPartialSyncStatus[] = [
  ...MITHRIL_PARTIAL_SYNC_WORKING_STATUSES,
  ...MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES.filter(
    (status) => status !== 'idle'
  ),
];

export const isMithrilPartialSyncWorkingStatus = (
  status: MithrilPartialSyncStatus
): boolean => MITHRIL_PARTIAL_SYNC_WORKING_STATUSES.includes(status);

export const isMithrilPartialSyncTerminalStatus = (
  status: MithrilPartialSyncStatus
): boolean => MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES.includes(status);

export const isMithrilPartialSyncOverlayStatus = (
  status: MithrilPartialSyncStatus
): boolean => MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES.includes(status);

export const isMithrilPartialSyncActiveStatus = (
  status: MithrilPartialSyncStatus
): boolean => status !== 'idle';

export const isMithrilPartialSyncRestoreCompleteStatus = (
  status: MithrilPartialSyncStatus
): boolean => status === 'completed' || status === 'starting-node';

export const isMithrilPartialSyncBlockingNodeStart = (
  status: MithrilPartialSyncStatus
): boolean =>
  [
    'stopping-node',
    'cancelling',
    'preparing',
    'downloading',
    'verifying',
    'converting',
    'installing',
    'finalizing',
    'starting-node',
  ].includes(status);
