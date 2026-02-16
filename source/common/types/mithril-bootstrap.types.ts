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

export type MithrilBootstrapError = {
  message: string;
  code?: string;
  logPath?: string;
};

export type MithrilBootstrapStatusUpdate = {
  status: MithrilBootstrapStatus;
  progress: number;
  currentStep?: string;
  snapshot?: MithrilSnapshotItem | null;
  error?: MithrilBootstrapError | null;
};
