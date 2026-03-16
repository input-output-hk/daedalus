export type MithrilBootstrapDecision = 'accept' | 'decline';

export type MithrilBootstrapStatus =
  | 'idle'
  | 'decision'
  | 'preparing'
  | 'downloading'
  | 'unpacking'
  | 'finalizing'
  | 'converting'
  | 'completed'
  | 'failed'
  | 'cancelled';

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
  progress: number;
  snapshot?: MithrilSnapshotItem | null;
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  remainingSeconds?: number;
  error?: MithrilBootstrapError | null;
};

export type ChainStorageConfig = {
  customPath: string | null;
  defaultPath: string;
  availableSpaceBytes: number;
  requiredSpaceBytes: number;
  setAt?: string;
};

export type ChainStorageValidationReason =
  | 'not-writable'
  | 'insufficient-space'
  | 'inside-state-dir'
  | 'path-not-found'
  | 'unknown';

export type ChainStorageValidation = {
  isValid: boolean;
  path: string | null;
  resolvedPath?: string;
  availableSpaceBytes?: number;
  requiredSpaceBytes?: number;
  reason?: ChainStorageValidationReason;
  message?: string;
};
