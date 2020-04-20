// @flow
import { Byron, Icarus, newPublicId } from 'cardano-js/dist/hd';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_WASM_BINARY_CHANNEL } from '../../common/ipc/api';
import type {
  getRecoveryWalletIdRendererRequest,
  getRecoveryWalletIdMainResponse,
} from '../../common/ipc/api';

const getRecoveryWalletIdChannel: MainIpcChannel<
  getRecoveryWalletIdRendererRequest,
  getRecoveryWalletIdMainResponse
> = new MainIpcChannel(GET_WASM_BINARY_CHANNEL);

export default () => {
  getRecoveryWalletIdChannel.onRequest(
    async (recoveryPhrase: Array<string>) => {
      let xprv;
      let cc;
      if (recoveryPhrase.length === 12) {
        [xprv, cc] = await Byron.generateMasterKey(recoveryPhrase);
      } else {
        [xprv, cc] = await Icarus.generateMasterKey(recoveryPhrase);
      }
      const walletId = newPublicId(xprv.to_public(), cc);
      return Promise.resolve(walletId);
    }
  );
};
