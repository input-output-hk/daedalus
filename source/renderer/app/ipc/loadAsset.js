// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { LOAD_ASSET_CHANNEL } from '../../../common/ipc/api';
import type {
  LoadAssetRendererRequest,
  LoadAssetMainResponse
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
type LoadAssetChannel = RendererIpcChannel<LoadAssetMainResponse, LoadAssetRendererRequest>
export const loadAssetChannel: LoadAssetChannel = new RendererIpcChannel(LOAD_ASSET_CHANNEL);
