// @flow
// import { Byron, Icarus, newPublicId } from 'cardano-js/dist/hd';
import fs from 'fs';
import path from 'path';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_WASM_BINARY_CHANNEL } from '../../common/ipc/api';
import type {
  getWasmBynaryRendererRequest,
  getWasmBynaryMainResponse,
} from '../../common/ipc/api';

const getWasmBynaryChannel: MainIpcChannel<
  getWasmBynaryRendererRequest,
  getWasmBynaryMainResponse
> = new MainIpcChannel(GET_WASM_BINARY_CHANNEL);

export default () => {
  getWasmBynaryChannel.onRequest(() => {
    const filePath = path.join(
      __dirname,
      '../../',
      'node_modules/cardano-js/lib/wasm',
      'js_chain_libs_bg.wasm'
    );
    const wasmBynary: Uint8Array = fs.readFileSync(filePath);
    return Promise.resolve(wasmBynary);
  });
};
