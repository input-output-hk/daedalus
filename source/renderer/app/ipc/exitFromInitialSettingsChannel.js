// @flow
import { EXIT_FROM_INITIAL_SETTINGS_CHANNEL } from '../../../common/ipc/api';
import type {
  ExitFromInitialSettingsMainResponse,
  ExitFromInitialSettingsRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const exitFromInitialSettingsChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ExitFromInitialSettingsMainResponse,
  ExitFromInitialSettingsRendererRequest
> = new RendererIpcChannel(EXIT_FROM_INITIAL_SETTINGS_CHANNEL);
