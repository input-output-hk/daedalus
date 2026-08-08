import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  EXPORT_WALLETS_CHANNEL,
  ExportWalletsRendererRequest,
  ExportWalletsMainResponse,
} from '../../../common/ipc/api';

export const exportWalletsChannel: RendererIpcChannel<
  ExportWalletsRendererRequest,
  ExportWalletsMainResponse
> = new RendererIpcChannel(EXPORT_WALLETS_CHANNEL);
