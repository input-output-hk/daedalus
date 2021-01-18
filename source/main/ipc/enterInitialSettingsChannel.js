// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { ENTER_INITIAL_SETTINGS_CHANNEL } from '../../common/ipc/api';
import type {
  EnterInitialSettingsMainResponse,
  EnterInitialSettingsRendererRequest,
} from '../../common/ipc/api';

export const enterInitialSettingsChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  EnterInitialSettingsRendererRequest,
  EnterInitialSettingsMainResponse
> = new MainIpcChannel(ENTER_INITIAL_SETTINGS_CHANNEL);
