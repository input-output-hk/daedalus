export type MithrilBootstrapDecision = 'accept' | 'decline';

export type MithrilBootstrapStatus =
  | 'idle'
  | 'decision'
  | 'preparing'
  | 'downloading'
  | 'verifying'
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
  currentStep?: string;
  snapshot?: MithrilSnapshotItem | null;
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  remainingSeconds?: number;
  error?: MithrilBootstrapError | null;
};
