// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_WASM_BINARY_CHANNEL } from '../../../common/ipc/api';
import type {
  getWasmBynaryRendererRequest,
  getWasmBynaryMainResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getWasmBynaryChannel: RendererIpcChannel<
  getWasmBynaryMainResponse,
  getWasmBynaryRendererRequest
> = new RendererIpcChannel(GET_WASM_BINARY_CHANNEL);
