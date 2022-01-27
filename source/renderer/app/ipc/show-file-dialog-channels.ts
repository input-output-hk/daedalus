import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  SHOW_OPEN_DIALOG_CHANNEL,
  SHOW_SAVE_DIALOG_CHANNEL,
} from '../../../common/ipc/api';
import type {
  ShowOpenDialogMainResponse,
  ShowOpenDialogRendererRequest,
  ShowSaveDialogMainResponse,
  ShowSaveDialogRendererRequest,
} from '../../../common/ipc/api';

export const showOpenDialogChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ShowOpenDialogMainResponse,
  ShowOpenDialogRendererRequest
> = new RendererIpcChannel(SHOW_OPEN_DIALOG_CHANNEL);
export const showSaveDialogChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ShowSaveDialogMainResponse,
  ShowSaveDialogRendererRequest
> = new RendererIpcChannel(SHOW_SAVE_DIALOG_CHANNEL);
