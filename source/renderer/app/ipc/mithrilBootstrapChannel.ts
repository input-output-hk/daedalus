import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_BOOTSTRAP_DECISION_CHANNEL,
  MITHRIL_BOOTSTRAP_START_CHANNEL,
  MITHRIL_BOOTSTRAP_STATUS_CHANNEL,
  MITHRIL_BOOTSTRAP_CANCEL_CHANNEL,
  MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL,
} from '../../../common/ipc/api';
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
} from '../../../common/ipc/api';

export const mithrilBootstrapDecisionChannel: RendererIpcChannel<
  MithrilBootstrapDecisionMainResponse,
  MithrilBootstrapDecisionRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_DECISION_CHANNEL);

export const mithrilBootstrapStartChannel: RendererIpcChannel<
  MithrilBootstrapStartMainResponse,
  MithrilBootstrapStartRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_START_CHANNEL);

export const mithrilBootstrapStatusChannel: RendererIpcChannel<
  MithrilBootstrapStatusMainResponse,
  MithrilBootstrapStatusRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_STATUS_CHANNEL);

export const mithrilBootstrapCancelChannel: RendererIpcChannel<
  MithrilBootstrapCancelMainResponse,
  MithrilBootstrapCancelRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_CANCEL_CHANNEL);

export const mithrilBootstrapSnapshotsChannel: RendererIpcChannel<
  MithrilBootstrapSnapshotsMainResponse,
  MithrilBootstrapSnapshotsRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL);
