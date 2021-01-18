// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { EXIT_FROM_INITIAL_SETTINGS_CHANNEL } from '../../common/ipc/api';
import type {
  ExitFromInitialSettingsMainResponse,
  ExitFromInitialSettingsRendererRequest,
} from '../../common/ipc/api';

export const exitFromInitialSettingsChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ExitFromInitialSettingsRendererRequest,
  ExitFromInitialSettingsMainResponse
> = new MainIpcChannel(EXIT_FROM_INITIAL_SETTINGS_CHANNEL);
