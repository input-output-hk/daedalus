// @flow
import { SUBMIT_BUG_REPORT_REQUEST_CHANNEL } from '../../../common/ipc/api';
import type {
  SubmitBugReportRequestResponse,
  SubmitBugReportRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const bugReportRequestChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  SubmitBugReportRequestResponse,
  SubmitBugReportRequest
> = new RendererIpcChannel(SUBMIT_BUG_REPORT_REQUEST_CHANNEL);
