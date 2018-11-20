// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { LOAD_ASSET_CHANNEL } from '../../../common/ipc/channels';
import type { LoadAssetRequest} from '../../../common/ipc/contracts';
import type { LoadAssetResponse } from '../../../common/ipc/contracts';

// IpcChannel<Incoming, Outgoing>

type LoadAssetChannel = RendererIpcChannel<LoadAssetResponse, LoadAssetRequest>
export const loadAssetChannel: LoadAssetChannel = new RendererIpcChannel(LOAD_ASSET_CHANNEL);
