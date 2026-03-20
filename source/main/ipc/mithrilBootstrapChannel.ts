import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MITHRIL_BOOTSTRAP_DECISION_CHANNEL,
  MITHRIL_BOOTSTRAP_START_CHANNEL,
  MITHRIL_BOOTSTRAP_STATUS_CHANNEL,
  MITHRIL_BOOTSTRAP_CANCEL_CHANNEL,
  MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL,
} from '../../common/ipc/api';
import type {
  MithrilBootstrapDecisionRendererRequest,
  MithrilBootstrapDecisionMainResponse,
  MithrilBootstrapStartRendererRequest,
  MithrilBootstrapStartMainResponse,
  MithrilBootstrapStatusRendererRequest,
  MithrilBootstrapStatusMainResponse,
  MithrilBootstrapCancelRendererRequest,
  MithrilBootstrapCancelMainResponse,
  MithrilBootstrapSnapshotsRendererRequest,
  MithrilBootstrapSnapshotsMainResponse,
} from '../../common/ipc/api';
import type {
  MithrilBootstrapDecision,
  MithrilBootstrapStatusUpdate,
} from '../../common/types/mithril-bootstrap.types';
import { MithrilBootstrapService } from '../mithril/MithrilBootstrapService';
import { ChainStorageManager } from '../utils/chainStorageManager';
import { logger } from '../utils/logging';

const mithrilBootstrapDecisionChannel: MainIpcChannel<
  MithrilBootstrapDecisionRendererRequest,
  MithrilBootstrapDecisionMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_DECISION_CHANNEL);
const mithrilBootstrapStartChannel: MainIpcChannel<
  MithrilBootstrapStartRendererRequest,
  MithrilBootstrapStartMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_START_CHANNEL);
export const mithrilBootstrapStatusChannel: MainIpcChannel<
  MithrilBootstrapStatusRendererRequest,
  MithrilBootstrapStatusMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_STATUS_CHANNEL);
const mithrilBootstrapCancelChannel: MainIpcChannel<
  MithrilBootstrapCancelRendererRequest,
  MithrilBootstrapCancelMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_CANCEL_CHANNEL);
const mithrilBootstrapSnapshotsChannel: MainIpcChannel<
  MithrilBootstrapSnapshotsRendererRequest,
  MithrilBootstrapSnapshotsMainResponse
> = new MainIpcChannel(MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL);

let pendingDecision: MithrilBootstrapDecision | null = null;
let decisionWaiters: Array<(decision: MithrilBootstrapDecision) => void> = [];
let statusListeners: Array<(status: MithrilBootstrapStatusUpdate) => void> = [];
let decisionListeners: Array<(decision: MithrilBootstrapDecision) => void> = [];

let lastStatus: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  snapshot: null,
  error: null,
};

export const getPendingMithrilBootstrapDecision = () => pendingDecision;
export const getMithrilBootstrapStatus = () => lastStatus;
export const onMithrilBootstrapStatus = (
  handler: (status: MithrilBootstrapStatusUpdate) => void
) => {
  statusListeners.push(handler);
  return () => {
    statusListeners = statusListeners.filter(
      (listener) => listener !== handler
    );
  };
};

export const onMithrilBootstrapDecision = (
  handler: (decision: MithrilBootstrapDecision) => void
) => {
  decisionListeners.push(handler);
  return () => {
    decisionListeners = decisionListeners.filter(
      (listener) => listener !== handler
    );
  };
};

export const setMithrilBootstrapStatus = (
  update: Partial<MithrilBootstrapStatusUpdate>
) => {
  lastStatus = {
    ...lastStatus,
    ...update,
  };
  return lastStatus;
};

export const waitForMithrilBootstrapDecision = (): Promise<
  MithrilBootstrapDecision
> => {
  if (pendingDecision) return Promise.resolve(pendingDecision);
  return new Promise((resolve) => {
    decisionWaiters.push(resolve);
  });
};

export const handleMithrilBootstrapRequests = (window: BrowserWindow) => {
  const service = new MithrilBootstrapService();
  const chainStorageManager = new ChainStorageManager();
  lastStatus = service.status;

  const syncWorkDir = async () => {
    const workDir = await chainStorageManager.resolveMithrilWorkDir();
    service.setWorkDir(workDir);
  };

  service.onStatus((status) => {
    lastStatus = status;
    mithrilBootstrapStatusChannel.send(status, window.webContents);
    statusListeners.forEach((listener) => listener(status));
  });

  mithrilBootstrapStatusChannel.onRequest(async () => lastStatus);

  mithrilBootstrapSnapshotsChannel.onRequest(async () =>
    service.listSnapshots()
  );

  mithrilBootstrapDecisionChannel.onRequest(async ({ decision }) => {
    pendingDecision = decision;
    decisionWaiters.forEach((resolve) => resolve(decision));
    decisionWaiters = [];
    decisionListeners.forEach((listener) => listener(decision));
    if (decision === 'decline') {
      if (lastStatus.status === 'failed') {
        return;
      }
      const update = setMithrilBootstrapStatus({
        status: 'idle',
        snapshot: null,
        error: null,
        elapsedSeconds: undefined,
      });
      await mithrilBootstrapStatusChannel.send(update, window.webContents);
      statusListeners.forEach((listener) => listener(update));
    }
  });

  mithrilBootstrapStartChannel.onRequest(async ({ digest, wipeChain }) => {
    try {
      await syncWorkDir();
      await service.startBootstrap(digest, { wipeChain });
    } catch (error) {
      logger.error('Mithril bootstrap failed to start', error);
      throw error;
    }
  });

  mithrilBootstrapCancelChannel.onRequest(async () => {
    await syncWorkDir();
    await service.cancel();
  });
};
