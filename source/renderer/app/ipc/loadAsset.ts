import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { LOAD_ASSET_CHANNEL } from '../../../common/ipc/api';
import type {
  LoadAssetRendererRequest,
  LoadAssetMainResponse,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const loadAssetChannel: RendererIpcChannel<
  LoadAssetMainResponse,
  LoadAssetRendererRequest
> = new RendererIpcChannel(LOAD_ASSET_CHANNEL);
