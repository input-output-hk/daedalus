// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { LOAD_ASSET_CHANNEL } from '../../../common/ipc/load-asset';
import type { LoadAssetRequest, LoadAssetResponse } from '../../../common/ipc/load-asset';

// IpcChannel<Incoming, Outgoing>

type LoadAssetChannel = RendererIpcChannel<LoadAssetResponse, LoadAssetRequest>
export const loadAssetChannel: LoadAssetChannel = new RendererIpcChannel(LOAD_ASSET_CHANNEL);
