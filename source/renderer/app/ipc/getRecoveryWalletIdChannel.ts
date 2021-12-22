import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_WASM_BINARY_CHANNEL } from '../../../common/ipc/api';
import type {
  getRecoveryWalletIdRendererRequest,
  getRecoveryWalletIdMainResponse,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getRecoveryWalletIdChannel: RendererIpcChannel<
  getRecoveryWalletIdMainResponse,
  getRecoveryWalletIdRendererRequest
> = new RendererIpcChannel(GET_WASM_BINARY_CHANNEL);
