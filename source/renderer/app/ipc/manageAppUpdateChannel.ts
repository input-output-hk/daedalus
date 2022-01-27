import { MANAGE_APP_UPDATE } from '../../../common/ipc/api';
import type {
  ManageAppUpdateRendererRequest,
  ManageAppUpdateMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const manageAppUpdateChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ManageAppUpdateMainResponse,
  ManageAppUpdateRendererRequest
> = new RendererIpcChannel(MANAGE_APP_UPDATE);
