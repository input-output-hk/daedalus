// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { LOAD_ASSET_CHANNEL } from '../../../common/ipc/load-asset';
import type { LoadAssetRequest, LoadAssetResponse } from '../../../common/ipc/load-asset';

// IpcChannel<Request, AwaitedResponse, ReceivedRequest, Response>

type LoadAssetChannel = RendererIpcChannel<LoadAssetRequest, LoadAssetResponse, void, void>
export const loadAssetChannel: LoadAssetChannel = new RendererIpcChannel(LOAD_ASSET_CHANNEL);
