import { MainIpcChannel } from './lib/MainIpcChannel';
import type {
  ShowUiPartMainRequest,
  ShowUiPartRendererResponse,
  ToggleUiPartMainRequest,
  ToggleUiPartRendererResponse,
} from '../../common/ipc/api';
import {
  SHOW_UI_PART_CHANNEL,
  TOGGLE_UI_PART_CHANNEL,
} from '../../common/ipc/api';

export const showUiPartChannel: MainIpcChannel<
  ShowUiPartRendererResponse,
  ShowUiPartMainRequest
> = new MainIpcChannel(SHOW_UI_PART_CHANNEL);
export const toggleUiPartChannel: MainIpcChannel<
  ToggleUiPartRendererResponse,
  ToggleUiPartMainRequest
> = new MainIpcChannel(TOGGLE_UI_PART_CHANNEL);
