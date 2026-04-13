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
import { isMithrilBootstrapBlockingNodeStart } from '../../common/types/mithril-bootstrap.types';
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { logger } from '../utils/logging';
import {
  chainStorageCoordinator,
  getMithrilBootstrapService,
} from '../utils/chainStorageCoordinator';

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
let decisionWaiters: Array<{
  resolve: (decision: MithrilBootstrapDecision) => void;
  reject: (error: Error) => void;
}> = [];
let statusListeners: Array<(status: MithrilBootstrapStatusUpdate) => void> = [];
let decisionListeners: Array<(decision: MithrilBootstrapDecision) => void> = [];
let getNodeState: () => CardanoNodeState | null | undefined = () => undefined;
let sendStatusUpdate:
  | ((status: MithrilBootstrapStatusUpdate) => Promise<void>)
  | null = null;

let lastStatus: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  snapshot: null,
  error: null,
};

export class MithrilDecisionCancelledError extends Error {
  constructor() {
    super('Mithril bootstrap decision was cancelled.');
    this.name = 'MithrilDecisionCancelledError';
  }
}

export const isMithrilDecisionCancelledError = (
  error: unknown
): error is MithrilDecisionCancelledError =>
  error instanceof MithrilDecisionCancelledError;

const broadcastMithrilBootstrapStatus = async (
  status: MithrilBootstrapStatusUpdate
): Promise<void> => {
  lastStatus = status;
  if (sendStatusUpdate) {
    await sendStatusUpdate(status);
  }
  statusListeners.forEach((listener) => listener(status));
};

export const getPendingMithrilBootstrapDecision = () => pendingDecision;
export const getMithrilBootstrapStatus = () => lastStatus;
export const setMithrilBootstrapNodeStateProvider = (
  provider: () => CardanoNodeState | null | undefined
) => {
  getNodeState = provider;
};
export const isMithrilBootstrapNodeStartBlocked = () =>
  isMithrilBootstrapBlockingNodeStart(lastStatus.status);
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
  return new Promise((resolve, reject) => {
    decisionWaiters.push({ resolve, reject });
  });
};

export const resetMithrilDecisionState = (): void => {
  pendingDecision = null;
  const cancellationError = new MithrilDecisionCancelledError();
  decisionWaiters.forEach(({ reject }) => reject(cancellationError));
  decisionWaiters = [];

  const update = setMithrilBootstrapStatus({
    status: 'idle',
    snapshot: null,
    error: null,
    elapsedSeconds: undefined,
    filesDownloaded: undefined,
    filesTotal: undefined,
    ancillaryBytesDownloaded: undefined,
    ancillaryBytesTotal: undefined,
    progressItems: undefined,
  });

  broadcastMithrilBootstrapStatus(update).catch((error) => {
    logger.warn(
      'Failed to broadcast Mithril idle status during decision reset',
      {
        error,
      }
    );
  });
};

export const handleMithrilBootstrapRequests = (window: BrowserWindow) => {
  const service = getMithrilBootstrapService();
  lastStatus = service.status;
  sendStatusUpdate = async (status) => {
    await mithrilBootstrapStatusChannel.send(status, window.webContents);
  };

  chainStorageCoordinator.syncMithrilWorkDir().catch((error) => {
    logger.warn('Failed to sync Mithril work directory on IPC setup', {
      error,
    });
  });

  service.onStatus((status) => {
    broadcastMithrilBootstrapStatus(status).catch((error) => {
      logger.warn('Failed to broadcast Mithril status update', {
        error,
      });
    });
  });

  mithrilBootstrapStatusChannel.onRequest(async () => lastStatus);

  mithrilBootstrapSnapshotsChannel.onRequest(async () =>
    chainStorageCoordinator.listSnapshots()
  );

  mithrilBootstrapDecisionChannel.onRequest(async ({ decision }) => {
    logger.info('[MITHRIL] Received bootstrap decision', {
      decision,
      previousDecision: pendingDecision,
      status: lastStatus.status,
    });
    pendingDecision = decision;
    decisionWaiters.forEach(({ resolve }) => resolve(decision));
    decisionWaiters = [];
    decisionListeners.forEach((listener) => listener(decision));
    if (decision === 'decline') {
      if (lastStatus.status === 'failed') {
        logger.info(
          '[MITHRIL] Keeping failed status active while decline recovery starts'
        );
        return;
      }
      const update = setMithrilBootstrapStatus({
        status: 'idle',
        snapshot: null,
        error: null,
        elapsedSeconds: undefined,
      });
      await broadcastMithrilBootstrapStatus(update);
    }
  });

  mithrilBootstrapStartChannel.onRequest(async ({ digest, wipeChain }) => {
    try {
      await chainStorageCoordinator.startBootstrap(digest, {
        wipeChain,
        nodeState: getNodeState(),
      });
    } catch (error) {
      logger.error('Mithril bootstrap failed to start', error);
      throw error;
    }
  });

  mithrilBootstrapCancelChannel.onRequest(async () => {
    await chainStorageCoordinator.cancelBootstrap();
  });
};
