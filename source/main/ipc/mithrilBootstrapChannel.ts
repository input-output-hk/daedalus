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

let lastStatus: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  progress: 0,
  currentStep: undefined,
  snapshot: null,
  error: null,
};

export const getPendingMithrilBootstrapDecision = () => pendingDecision;

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
  lastStatus = service.status;

  service.onStatus((status) => {
    lastStatus = status;
    mithrilBootstrapStatusChannel.send(status, window.webContents);
  });

  mithrilBootstrapStatusChannel.onRequest(async () => lastStatus);

  mithrilBootstrapSnapshotsChannel.onRequest(async () =>
    service.listSnapshots()
  );

  mithrilBootstrapDecisionChannel.onRequest(async ({ decision }) => {
    pendingDecision = decision;
    decisionWaiters.forEach((resolve) => resolve(decision));
    decisionWaiters = [];
  });

  mithrilBootstrapStartChannel.onRequest(async ({ digest }) => {
    try {
      await service.startBootstrap(digest);
    } catch (error) {
      logger.error('Mithril bootstrap failed to start', error);
      throw error;
    }
  });

  mithrilBootstrapCancelChannel.onRequest(async () => {
    await service.cancel();
  });
};
