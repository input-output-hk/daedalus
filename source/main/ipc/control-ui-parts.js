// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import type { ShowUiPartRequest, ToggleUiPartRequest } from '../../common/ipc/api';
import { SHOW_UI_PART_CHANNEL, TOGGLE_UI_PART_CHANNEL } from '../../common/ipc/api';

export const showUiPartChannel: (
  MainIpcChannel<void, ShowUiPartRequest>
) = new MainIpcChannel(SHOW_UI_PART_CHANNEL);

export const toggleUiPartChannel: (
  MainIpcChannel<void, ToggleUiPartRequest>
) = new MainIpcChannel(TOGGLE_UI_PART_CHANNEL);
