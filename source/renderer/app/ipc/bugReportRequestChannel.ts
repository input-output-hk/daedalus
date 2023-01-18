import { SUBMIT_BUG_REPORT_REQUEST_CHANNEL } from '../../../common/ipc/api';
import type {
  SubmitBugReportRequestMainResponse,
  SubmitBugReportRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const bugReportRequestChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  SubmitBugReportRequestMainResponse,
  SubmitBugReportRendererRequest
> = new RendererIpcChannel(SUBMIT_BUG_REPORT_REQUEST_CHANNEL);
