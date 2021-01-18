// @flow
import { ENTER_INITIAL_SETTINGS_CHANNEL } from '../../../common/ipc/api';
import type {
  EnterInitialSettingsMainResponse,
  EnterInitialSettingsRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const enterInitialSettingsChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  EnterInitialSettingsMainResponse,
  EnterInitialSettingsRendererRequest
> = new RendererIpcChannel(ENTER_INITIAL_SETTINGS_CHANNEL);
