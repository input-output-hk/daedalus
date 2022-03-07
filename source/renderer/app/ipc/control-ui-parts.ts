import { RendererIpcChannel } from './lib/RendererIpcChannel';
import type {
  ShowUiPartMainRequest,
  ShowUiPartRendererResponse,
  ToggleUiPartMainRequest,
  ToggleUiPartRendererResponse,
} from '../../../common/ipc/api';
import {
  SHOW_UI_PART_CHANNEL,
  TOGGLE_UI_PART_CHANNEL,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const toggleUiPartChannel: RendererIpcChannel<
  ToggleUiPartMainRequest,
  ToggleUiPartRendererResponse
> = new RendererIpcChannel(TOGGLE_UI_PART_CHANNEL);
export const showUiPartChannel: RendererIpcChannel<
  ShowUiPartMainRequest,
  ShowUiPartRendererResponse
> = new RendererIpcChannel(SHOW_UI_PART_CHANNEL);
