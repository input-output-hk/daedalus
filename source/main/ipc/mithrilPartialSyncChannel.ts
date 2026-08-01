import type { BrowserWindow } from 'electron';
import { MITHRIL_AVAILABILITY_CHANNEL } from '../../common/ipc/api';
import type {
  MithrilSyncStatusMainResponse,
  MithrilAvailabilityMainResponse,
} from '../../common/ipc/api';
import { syncStatusChannel } from './mithrilBootstrapChannel';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
import { getMithrilController } from '../mithril/MithrilController';

const availabilityChannel = new MainIpcChannel<
  void,
  MithrilAvailabilityMainResponse
>(MITHRIL_AVAILABILITY_CHANNEL);

const partialSyncSnapshotToUnified = (
  snapshot: MithrilPartialSyncStatusSnapshot
): MithrilSyncStatusMainResponse => ({
  status: snapshot.status,
  flowType: 'partial-sync' as const,
  filesDownloaded: snapshot.transferProgress.filesDownloaded,
  filesTotal: snapshot.transferProgress.filesTotal,
  snapshotBytesDownloaded: snapshot.transferProgress.snapshotBytesDownloaded,
  snapshotBytesTotal: snapshot.transferProgress.snapshotBytesTotal,
  ancillaryBytesDownloaded: snapshot.transferProgress.ancillaryBytesDownloaded,
  ancillaryBytesTotal: snapshot.transferProgress.ancillaryBytesTotal,
  progressItems: snapshot.progressItems,
  error: snapshot.error
    ? {
        message: snapshot.error.message,
        code: snapshot.error.code,
        logPath: snapshot.error.logPath,
        stage: snapshot.error.stage,
      }
    : null,
  logPath: snapshot.logPath,
  allowedRecoveryActions: snapshot.allowedRecoveryActions,
});

// configureMithrilPartialSyncRuntime is called from index.ts; kept as a no-op.
export const configureMithrilPartialSyncRuntime = (_dependencies: {
  stopNode?: () => Promise<void>;
  restartStartupFlow?: () => Promise<void>;
}): void => {};

export const handleMithrilPartialSyncRequests = (window: BrowserWindow) => {
  const controller = getMithrilController();
  controller.setPartialSyncStatusSender(async (status) => {
    await syncStatusChannel
      .send(partialSyncSnapshotToUnified(status), window.webContents)
      .catch(() => {});
  });

  controller.setAvailabilitySender(async (availability) => {
    await availabilityChannel
      .send(availability, window.webContents)
      .catch(() => {});
  });

  controller.initialize();
};
