import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_SYNC_STATUS_CHANNEL,
  MITHRIL_SYNC_CANCEL_CHANNEL,
  MITHRIL_AVAILABILITY_CHANNEL,
} from '../../../common/ipc/api';
import type {
  MithrilSyncStatusRendererRequest,
  MithrilSyncStatusMainResponse,
  MithrilSyncCancelRendererRequest,
  MithrilSyncCancelMainResponse,
  MithrilAvailabilityRendererRequest,
  MithrilAvailabilityMainResponse,
} from '../../../common/ipc/api';

export const mithrilSyncStatusChannel: RendererIpcChannel<
  MithrilSyncStatusMainResponse,
  MithrilSyncStatusRendererRequest
> = new RendererIpcChannel(MITHRIL_SYNC_STATUS_CHANNEL);

export const mithrilSyncCancelChannel: RendererIpcChannel<
  MithrilSyncCancelMainResponse,
  MithrilSyncCancelRendererRequest
> = new RendererIpcChannel(MITHRIL_SYNC_CANCEL_CHANNEL);

export const mithrilAvailabilityChannel: RendererIpcChannel<
  MithrilAvailabilityMainResponse,
  MithrilAvailabilityRendererRequest
> = new RendererIpcChannel(MITHRIL_AVAILABILITY_CHANNEL);
