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
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { logger } from '../utils/logging';
import {
  getMithrilController,
  isMithrilDecisionCancelledError,
  MithrilDecisionCancelledError,
} from '../mithril/MithrilController';

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

export { isMithrilDecisionCancelledError, MithrilDecisionCancelledError };

export const getPendingMithrilBootstrapDecision = () =>
  getMithrilController().getPendingBootstrapDecision();
export const getMithrilBootstrapStatus = () =>
  getMithrilController().getBootstrapStatus();
export const getMithrilBootstrapNodeState = () =>
  getMithrilController().getNodeState();
export const setMithrilBootstrapNodeStateProvider = (
  provider: () => CardanoNodeState | null | undefined
) => {
  getMithrilController().setNodeStateProvider(provider);
};
export const isMithrilBootstrapNodeStartBlocked = () =>
  getMithrilController().isBootstrapNodeStartBlocked();
export const onMithrilBootstrapStatus = (
  handler: (status: MithrilBootstrapStatusUpdate) => void
) => getMithrilController().onBootstrapStatus(handler);

export const onMithrilBootstrapDecision = (
  handler: (decision: MithrilBootstrapDecision) => void
) => getMithrilController().onBootstrapDecision(handler);

export const setMithrilBootstrapStatus = (
  update: Partial<MithrilBootstrapStatusUpdate>
) => getMithrilController().setBootstrapStatus(update);

export const waitForMithrilBootstrapDecision =
  (): Promise<MithrilBootstrapDecision> =>
    getMithrilController().waitForBootstrapDecision();

export const resetMithrilDecisionState = (
  options: {
    suppressStatusBroadcast?: boolean;
  } = {}
): void => {
  getMithrilController().resetBootstrapDecisionState(options);
};

let mithrilBootstrapRequestsInitialized = false;

export const handleMithrilBootstrapRequests = (window: BrowserWindow) => {
  // Always rebind sendStatusUpdate to the latest window so status
  // targets the current webContents after window recreation.
  const controller = getMithrilController();
  controller.setBootstrapStatusSender(async (status) => {
    await mithrilBootstrapStatusChannel.send(status, window.webContents);
  });

  controller.initialize();

  if (mithrilBootstrapRequestsInitialized) return;
  mithrilBootstrapRequestsInitialized = true;

  mithrilBootstrapStatusChannel.onRequest(async () =>
    controller.getBootstrapStatus()
  );

  mithrilBootstrapSnapshotsChannel.onRequest(async () =>
    controller.listSnapshots()
  );

  mithrilBootstrapDecisionChannel.onRequest(async ({ decision }) => {
    await controller.submitBootstrapDecision(decision);
  });

  mithrilBootstrapStartChannel.onRequest(async ({ digest, wipeChain }) => {
    try {
      await controller.startBootstrap({ digest, wipeChain });
    } catch (error) {
      logger.error('Mithril bootstrap failed to start', error);
      throw error;
    }
  });

  mithrilBootstrapCancelChannel.onRequest(async () => {
    await controller.cancelBootstrap();
  });
};
