// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import type {
  ShowUiPartRequest,
  ToggleUiPartRequest,
} from '../../../common/ipc/api';
import {
  SHOW_UI_PART_CHANNEL,
  TOGGLE_UI_PART_CHANNEL,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const toggleUiPartChannel: RendererIpcChannel<
  ToggleUiPartRequest,
  void
> = new RendererIpcChannel(TOGGLE_UI_PART_CHANNEL);

export const showUiPartChannel: RendererIpcChannel<
  ShowUiPartRequest,
  void
> = new RendererIpcChannel(SHOW_UI_PART_CHANNEL);
