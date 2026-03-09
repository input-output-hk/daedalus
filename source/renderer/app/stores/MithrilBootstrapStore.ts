import { action, computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import type {
  MithrilBootstrapStatus,
  MithrilBootstrapStatusUpdate,
  MithrilSnapshotItem,
  MithrilBootstrapError,
  MithrilBootstrapDecision,
} from '../../../common/types/mithril-bootstrap.types';
import {
  mithrilBootstrapDecisionChannel,
  mithrilBootstrapStartChannel,
  mithrilBootstrapStatusChannel,
  mithrilBootstrapCancelChannel,
  mithrilBootstrapSnapshotsChannel,
} from '../ipc/mithrilBootstrapChannel';
import { logger } from '../utils/logging';

const DEFAULT_STATUS: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  progress: 0,
  currentStep: undefined,
  snapshot: null,
  error: null,
};

export default class MithrilBootstrapStore extends Store {
  @observable status: MithrilBootstrapStatus = DEFAULT_STATUS.status;
  @observable progress = DEFAULT_STATUS.progress;
  @observable currentStep: string | undefined = DEFAULT_STATUS.currentStep;
  @observable snapshot: MithrilSnapshotItem | null =
    DEFAULT_STATUS.snapshot ?? null;
  @observable filesDownloaded: number | undefined =
    DEFAULT_STATUS.filesDownloaded;
  @observable filesTotal: number | undefined = DEFAULT_STATUS.filesTotal;
  @observable elapsedSeconds: number | undefined =
    DEFAULT_STATUS.elapsedSeconds;
  @observable remainingSeconds: number | undefined =
    DEFAULT_STATUS.remainingSeconds;
  @observable error: MithrilBootstrapError | null =
    DEFAULT_STATUS.error ?? null;
  @observable snapshots: Array<MithrilSnapshotItem> = [];
  @observable isFetchingSnapshots = false;

  @computed
  get bytesDownloaded(): number | undefined {
    if (
      this.snapshot == null ||
      typeof this.snapshot.size !== 'number' ||
      this.snapshot.size <= 0 ||
      this.filesDownloaded == null ||
      this.filesTotal == null ||
      this.filesTotal <= 0
    ) {
      return undefined;
    }

    const normalizedFilesDownloaded = Math.min(
      Math.max(this.filesDownloaded, 0),
      this.filesTotal
    );

    return Math.round(
      (normalizedFilesDownloaded / this.filesTotal) * this.snapshot.size
    );
  }

  @computed
  get throughputBps(): number | undefined {
    if (
      this.bytesDownloaded == null ||
      this.elapsedSeconds == null ||
      this.elapsedSeconds <= 0
    ) {
      return undefined;
    }

    return this.bytesDownloaded / this.elapsedSeconds;
  }

  setup() {
    mithrilBootstrapStatusChannel.onReceive(this._updateStatus);
    this.syncStatus().catch((error) => {
      logger.warn('MithrilBootstrapStore: failed to sync status', { error });
    });
  }

  @action
  syncStatus = async () => {
    const status =
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
      await mithrilBootstrapStatusChannel.request();
    this._updateStatus(status);
  };

  @action
  _updateStatus = (update: MithrilBootstrapStatusUpdate): Promise<void> => {
    this.status = update.status;
    if (typeof update.progress === 'number') {
      this.progress = update.progress;
    }
    if ('currentStep' in update) {
      this.currentStep = update.currentStep;
    }
    if ('snapshot' in update) {
      this.snapshot = update.snapshot ?? null;
    }
    if ('filesDownloaded' in update) {
      this.filesDownloaded = update.filesDownloaded;
    }
    if ('filesTotal' in update) {
      this.filesTotal = update.filesTotal;
    }
    if ('elapsedSeconds' in update) {
      this.elapsedSeconds = update.elapsedSeconds;
    }
    if ('remainingSeconds' in update) {
      this.remainingSeconds = update.remainingSeconds;
    }
    if ('error' in update) {
      this.error = update.error ?? null;
    }
    return Promise.resolve();
  };

  @action
  loadSnapshots = async () => {
    this.isFetchingSnapshots = true;
    try {
      const snapshots =
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
        await mithrilBootstrapSnapshotsChannel.request();
      runInAction('load Mithril snapshots', () => {
        this.snapshots = snapshots || [];
      });
    } catch (error) {
      logger.warn('MithrilBootstrapStore: failed to load snapshots', { error });
    } finally {
      runInAction('finish loading Mithril snapshots', () => {
        this.isFetchingSnapshots = false;
      });
    }
  };

  @action
  setDecision = async (decision: MithrilBootstrapDecision) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 1.
    await mithrilBootstrapDecisionChannel.request({ decision });
  };

  @action
  startBootstrap = async (
    digest?: string,
    options?: {
      wipeChain?: boolean;
    }
  ) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 1.
    await mithrilBootstrapStartChannel.request({
      digest,
      wipeChain: options?.wipeChain,
    });
  };

  @action
  cancelBootstrap = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0.
    await mithrilBootstrapCancelChannel.request();
  };
}
