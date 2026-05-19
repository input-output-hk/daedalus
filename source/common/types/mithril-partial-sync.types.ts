import type { MithrilProgressItem } from './mithril-bootstrap.types';

export type MithrilPartialSyncStatus =
  | 'idle'
  | 'stopping-node'
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

export type MithrilPartialSyncError = {
  message: string;
  code?: string;
  logPath?: string;
  stage?: MithrilPartialSyncErrorStage;
};

export type MithrilPartialSyncStatusUpdate = {
  status: MithrilPartialSyncStatus;
  allowedRecoveryActions: MithrilPartialSyncFailureAction[];
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  progressItems?: MithrilProgressItem[];
  error?: MithrilPartialSyncError | null;
  logPath?: string;
};

export const isMithrilPartialSyncTerminalStatus = (
  status: MithrilPartialSyncStatus
): boolean => ['completed', 'failed', 'cancelled'].includes(status);

export const isMithrilPartialSyncRestoreCompleteStatus = (
  status: MithrilPartialSyncStatus
): boolean => status === 'completed' || status === 'starting-node';

export const isMithrilPartialSyncBlockingNodeStart = (
  status: MithrilPartialSyncStatus
): boolean =>
  [
    'stopping-node',
    'preparing',
    'downloading',
    'verifying',
    'converting',
    'installing',
    'finalizing',
    'starting-node',
  ].includes(status);
