export type MithrilBootstrapDecision = 'accept' | 'decline';

export type MithrilProgressItemState =
  | 'pending'
  | 'active'
  | 'completed'
  | 'error';

export type MithrilProgressItem = {
  id: string;
  label: string;
  state: MithrilProgressItemState;
  timestamp?: string;
};

export type MithrilBootstrapStatus =
  | 'idle'
  | 'decision'
  | 'preparing'
  | 'downloading'
  | 'verifying'
  | 'unpacking'
  | 'finalizing'
  | 'converting'
  | 'completed'
  | 'starting-node'
  | 'failed'
  | 'cancelled';

export const isMithrilBootstrapRestoreCompleteStatus = (
  status: MithrilBootstrapStatus
): boolean => status === 'completed' || status === 'starting-node';

export const isMithrilBootstrapBlockingNodeStart = (
  status: MithrilBootstrapStatus
): boolean =>
  [
    'decision',
    'preparing',
    'downloading',
    'verifying',
    'unpacking',
    'finalizing',
    'converting',
    'completed',
    'starting-node',
  ].includes(status);

export type MithrilSnapshotItem = {
  digest: string;
  createdAt: string;
  size: number;
  cardanoNodeVersion?: string;
  network?: string;
};

export type MithrilBootstrapErrorStage =
  | 'download'
  | 'verify'
  | 'convert'
  | 'node-start';

export type MithrilBootstrapError = {
  message: string;
  code?: string;
  logPath?: string;
  stage?: MithrilBootstrapErrorStage;
};

export type MithrilBootstrapStatusUpdate = {
  status: MithrilBootstrapStatus;
  snapshot?: MithrilSnapshotItem | null;
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  error?: MithrilBootstrapError | null;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  progressItems?: MithrilProgressItem[];
};

export type ChainStorageConfig = {
  customPath: string | null;
  defaultPath: string;
  availableSpaceBytes: number;
  requiredSpaceBytes: number;
  setAt?: string;
};

export type ChainSubdirectoryStatus =
  | 'will-create'
  | 'existing-directory'
  | 'path-is-file';

export type ChainStorageValidationReason =
  | 'not-writable'
  | 'insufficient-space'
  | 'inside-state-dir'
  | 'is-managed-child'
  | 'path-not-found'
  | 'path-is-file'
  | 'unknown';

export type ChainStorageValidation = {
  isValid: boolean;
  path: string | null;
  resolvedPath?: string;
  availableSpaceBytes?: number;
  requiredSpaceBytes?: number;
  chainSubdirectoryStatus?: ChainSubdirectoryStatus;
  reason?: ChainStorageValidationReason;
  message?: string;
};
