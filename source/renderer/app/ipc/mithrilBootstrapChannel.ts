import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_BOOTSTRAP_DECISION_CHANNEL,
  MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL,
} from '../../../common/ipc/api';
import type {
  MithrilBootstrapDecisionRendererRequest,
  MithrilBootstrapDecisionMainResponse,
  MithrilBootstrapSnapshotsRendererRequest,
  MithrilBootstrapSnapshotsMainResponse,
} from '../../../common/ipc/api';

export const mithrilBootstrapDecisionChannel: RendererIpcChannel<
  MithrilBootstrapDecisionMainResponse,
  MithrilBootstrapDecisionRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_DECISION_CHANNEL);

export const mithrilBootstrapSnapshotsChannel: RendererIpcChannel<
  MithrilBootstrapSnapshotsMainResponse,
  MithrilBootstrapSnapshotsRendererRequest
> = new RendererIpcChannel(MITHRIL_BOOTSTRAP_SNAPSHOTS_CHANNEL);
