import { TOGGLE_RTS_FLAGS_MODE_CHANNEL } from '../../../common/ipc/api';
import type {
  ToggleRTSFlagsModeMainResponse,
  ToggleRTSFlagsModeRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const toggleRTSFlagsModeChannel: RendererIpcChannel<
  ToggleRTSFlagsModeRendererRequest,
  ToggleRTSFlagsModeMainResponse
> = new RendererIpcChannel(TOGGLE_RTS_FLAGS_MODE_CHANNEL);
