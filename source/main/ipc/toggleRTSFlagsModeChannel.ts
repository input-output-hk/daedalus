import { TOGGLE_RTS_FLAGS_MODE_CHANNEL } from '../../common/ipc/api';
import type {
  ToggleRTSFlagsModeMainResponse,
  ToggleRTSFlagsModeRendererRequest,
} from '../../common/ipc/api';
import { MainIpcChannel } from './lib/MainIpcChannel';

export const toggleRTSFlagsModeChannel: MainIpcChannel<
  ToggleRTSFlagsModeRendererRequest,
  ToggleRTSFlagsModeMainResponse
> = new MainIpcChannel(TOGGLE_RTS_FLAGS_MODE_CHANNEL);
