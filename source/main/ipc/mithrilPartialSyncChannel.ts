import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MITHRIL_PARTIAL_SYNC_START_CHANNEL,
  MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL,
  MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL,
  MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL,
  MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL,
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
} from '../../common/ipc/api';
import type { MithrilPartialSyncStatusUpdate } from '../../common/types/mithril-partial-sync.types';
import { logger } from '../utils/logging';
import { getMithrilBootstrapNodeState } from './mithrilBootstrapChannel';
import { MithrilPartialSyncService } from '../mithril/MithrilPartialSyncService';
import { chainStorageCoordinator } from '../utils/chainStorageCoordinator';

let isPartialSyncActive: () => boolean = () => false;

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

let statusListeners: Array<(status: MithrilPartialSyncStatusUpdate) => void> = [];
let sendStatusUpdate:
  | ((status: MithrilPartialSyncStatusUpdate) => Promise<void>)
  | null = null;

let lastStatus: MithrilPartialSyncStatusUpdate = {
  status: 'idle',
  allowedRecoveryActions: [],
  error: null,
};

const PARTIAL_SYNC_NOT_IMPLEMENTED_ERROR =
  'Mithril partial sync action is not implemented yet.';

const rejectUntilImplemented = async (): Promise<void> => {
  throw new Error(PARTIAL_SYNC_NOT_IMPLEMENTED_ERROR);
};

const broadcastMithrilPartialSyncStatus = async (
  status: MithrilPartialSyncStatusUpdate
): Promise<void> => {
  lastStatus = status;
  statusListeners.forEach((listener) => listener(status));
  try {
    if (sendStatusUpdate) {
      await sendStatusUpdate(status);
    }
  } catch (error) {
    logger.warn('Failed to send Mithril partial sync status to renderer', {
      error,
    });
  }
};

export const getMithrilPartialSyncStatus = () => lastStatus;

export const isMithrilPartialSyncActive = () => isPartialSyncActive();

export const setMithrilPartialSyncActiveProvider = (provider: () => boolean) => {
  isPartialSyncActive = provider;
};

export const setMithrilPartialSyncStatus = (
  status: MithrilPartialSyncStatusUpdate
) => {
  lastStatus = status;
  return lastStatus;
};

export const onMithrilPartialSyncStatus = (
  handler: (status: MithrilPartialSyncStatusUpdate) => void
) => {
  statusListeners.push(handler);
  return () => {
    statusListeners = statusListeners.filter((listener) => listener !== handler);
  };
};

let mithrilPartialSyncRequestsInitialized = false;
const mithrilPartialSyncService = new MithrilPartialSyncService();

export const handleMithrilPartialSyncRequests = (window: BrowserWindow) => {
  sendStatusUpdate = async (status) => {
    await mithrilPartialSyncStatusChannel.send(status, window.webContents);
  };

  if (mithrilPartialSyncRequestsInitialized) return;
  mithrilPartialSyncRequestsInitialized = true;

  lastStatus = mithrilPartialSyncService.status;

  chainStorageCoordinator.setPartialSyncHandlers({
    start: async (context) => mithrilPartialSyncService.start(context),
    cancel: async () => mithrilPartialSyncService.cancel(),
  });

  mithrilPartialSyncService.onStatus((status) => {
    broadcastMithrilPartialSyncStatus(status).catch((error) => {
      logger.warn('Failed to broadcast Mithril partial sync status update', {
        error,
      });
    });
  });

  mithrilPartialSyncStatusChannel.onRequest(async () => lastStatus);
  mithrilPartialSyncStartChannel.onRequest(async () => {
    await chainStorageCoordinator.startPartialSync({
      nodeState: getMithrilBootstrapNodeState(),
    });
  });
  mithrilPartialSyncCancelChannel.onRequest(async () => {
    await chainStorageCoordinator.cancelPartialSync();
  });
  mithrilPartialSyncRestartNormalChannel.onRequest(rejectUntilImplemented);
  mithrilPartialSyncWipeAndFullSyncChannel.onRequest(rejectUntilImplemented);
};

export const emitMithrilPartialSyncStatus = async (
  status: MithrilPartialSyncStatusUpdate
): Promise<void> => {
  await broadcastMithrilPartialSyncStatus(status);
};

export const getMithrilPartialSyncNotImplementedError = () =>
  PARTIAL_SYNC_NOT_IMPLEMENTED_ERROR;
