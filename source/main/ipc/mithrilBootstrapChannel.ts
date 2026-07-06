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
import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { logger } from '../utils/logging';
import { getMithrilController } from '../mithril/MithrilController';

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

export const getMithrilBootstrapNodeState = () =>
  getMithrilController().getNodeState();
export const setMithrilBootstrapNodeStateProvider = (
  provider: () => CardanoNodeState | null | undefined
) => {
  getMithrilController().setNodeStateProvider(provider);
};

let mithrilBootstrapRequestsInitialized = false;

export const handleMithrilBootstrapRequests = (window: BrowserWindow) => {
  // Rebind the status sender to the latest window each call, so status targets the current webContents
  //  after window recreation.
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
