// @flow
import { ReportRequestChannelName } from '../../../common/ipc/api';
import type {
  ReportRequestMainResponse,
  ReportRequestRendererRequest
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const reportRequestChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<ReportRequestMainResponse, ReportRequestRendererRequest>
) = (
  new RendererIpcChannel(ReportRequestChannelName)
);
