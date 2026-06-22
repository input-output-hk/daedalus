import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MITHRIL_PARTIAL_SYNC_START_CHANNEL,
  MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL,
  MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL,
  MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL,
  MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL,
  MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL,
  MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL,
} from '../../common/ipc/api';
import type {
  MithrilPartialSyncStartRendererRequest,
  MithrilPartialSyncStartMainResponse,
  MithrilPartialSyncStatusRendererRequest,
  MithrilPartialSyncStatusMainResponse,
  MithrilPartialSyncCancelRendererRequest,
  MithrilPartialSyncCancelMainResponse,
  MithrilPartialSyncRestartNormalRendererRequest,
  MithrilPartialSyncRestartNormalMainResponse,
  MithrilPartialSyncWipeAndFullSyncRendererRequest,
  MithrilPartialSyncWipeAndFullSyncMainResponse,
  MithrilPartialSyncAvailabilityRendererRequest,
  MithrilPartialSyncAvailabilityMainResponse,
  MithrilPartialSyncFinalizeRendererRequest,
  MithrilPartialSyncFinalizeMainResponse,
} from '../../common/ipc/api';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';
import { getMithrilController } from '../mithril/MithrilController';

const mithrilPartialSyncStartChannel: MainIpcChannel<
  MithrilPartialSyncStartRendererRequest,
  MithrilPartialSyncStartMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_START_CHANNEL);

export const mithrilPartialSyncStatusChannel: MainIpcChannel<
  MithrilPartialSyncStatusRendererRequest,
  MithrilPartialSyncStatusMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL);

const mithrilPartialSyncCancelChannel: MainIpcChannel<
  MithrilPartialSyncCancelRendererRequest,
  MithrilPartialSyncCancelMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL);

const mithrilPartialSyncRestartNormalChannel: MainIpcChannel<
  MithrilPartialSyncRestartNormalRendererRequest,
  MithrilPartialSyncRestartNormalMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL);

const mithrilPartialSyncWipeAndFullSyncChannel: MainIpcChannel<
  MithrilPartialSyncWipeAndFullSyncRendererRequest,
  MithrilPartialSyncWipeAndFullSyncMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL);

const mithrilPartialSyncAvailabilityChannel: MainIpcChannel<
  MithrilPartialSyncAvailabilityRendererRequest,
  MithrilPartialSyncAvailabilityMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL);

const mithrilPartialSyncFinalizeChannel: MainIpcChannel<
  MithrilPartialSyncFinalizeRendererRequest,
  MithrilPartialSyncFinalizeMainResponse
> = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL);

export const getMithrilPartialSyncStatus = () =>
  getMithrilController().getPartialSyncStatus();

export const isMithrilPartialSyncActive = () =>
  getMithrilController().isPartialSyncActive();

export const setMithrilPartialSyncActiveProvider = (
  _provider: () => boolean
) => {};

export const setMithrilPartialSyncStatus = (
  status: MithrilPartialSyncStatusSnapshot
) => {
  getMithrilController().setPartialSyncStatus(status);
  return status;
};

export const onMithrilPartialSyncStatus = (
  handler: (status: MithrilPartialSyncStatusSnapshot) => void
) => {
  return getMithrilController().onPartialSyncStatus(handler);
};

let mithrilPartialSyncRequestsInitialized = false;

export const configureMithrilPartialSyncRuntime = (dependencies: {
  stopNode?: () => Promise<void>;
  restartStartupFlow?: () => Promise<void>;
}) => {
  getMithrilController().configurePartialSyncRuntime(dependencies);
};

export const handleMithrilPartialSyncRequests = (window: BrowserWindow) => {
  const controller = getMithrilController();
  controller.setPartialSyncStatusSender(async (status) => {
    await mithrilPartialSyncStatusChannel.send(status, window.webContents);
  });

  controller.initialize();

  if (mithrilPartialSyncRequestsInitialized) return;
  mithrilPartialSyncRequestsInitialized = true;

  mithrilPartialSyncStatusChannel.onRequest(async () =>
    controller.getPartialSyncStatus()
  );
  mithrilPartialSyncStartChannel.onRequest(async () => {
    await controller.startPartialSync();
  });
  mithrilPartialSyncCancelChannel.onRequest(async () => {
    await controller.cancelPartialSync();
  });
  mithrilPartialSyncRestartNormalChannel.onRequest(async () => {
    await controller.restartNormalFromPartialSync();
  });
  mithrilPartialSyncWipeAndFullSyncChannel.onRequest(async () => {
    await controller.wipeAndFullSyncFromPartialSync();
  });
  mithrilPartialSyncAvailabilityChannel.onRequest(async () =>
    controller.getPartialSyncAvailability()
  );
  mithrilPartialSyncFinalizeChannel.onRequest(async () => {
    await controller.finalizePartialSync();
  });
};

export const emitMithrilPartialSyncStatus = async (
  status: MithrilPartialSyncStatusSnapshot
): Promise<void> => {
  await getMithrilController().broadcastPartialSyncStatus(status);
};
