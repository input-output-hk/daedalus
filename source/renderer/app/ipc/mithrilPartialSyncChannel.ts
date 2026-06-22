import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_PARTIAL_SYNC_START_CHANNEL,
  MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL,
  MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL,
  MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL,
  MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL,
  MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL,
} from '../../../common/ipc/api';
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
} from '../../../common/ipc/api';

export const mithrilPartialSyncStartChannel: RendererIpcChannel<
  MithrilPartialSyncStartMainResponse,
  MithrilPartialSyncStartRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_START_CHANNEL);

export const mithrilPartialSyncStatusChannel: RendererIpcChannel<
  MithrilPartialSyncStatusMainResponse,
  MithrilPartialSyncStatusRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL);

export const mithrilPartialSyncCancelChannel: RendererIpcChannel<
  MithrilPartialSyncCancelMainResponse,
  MithrilPartialSyncCancelRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL);

export const mithrilPartialSyncRestartNormalChannel: RendererIpcChannel<
  MithrilPartialSyncRestartNormalMainResponse,
  MithrilPartialSyncRestartNormalRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL);

export const mithrilPartialSyncWipeAndFullSyncChannel: RendererIpcChannel<
  MithrilPartialSyncWipeAndFullSyncMainResponse,
  MithrilPartialSyncWipeAndFullSyncRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL);

export const mithrilPartialSyncAvailabilityChannel: RendererIpcChannel<
  MithrilPartialSyncAvailabilityMainResponse,
  MithrilPartialSyncAvailabilityRendererRequest
> = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_AVAILABILITY_CHANNEL);
