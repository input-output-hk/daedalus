// @flow
/**
 *
 * @verify-recovery-phrase TODO
 *
 * 👉 If `cardano-js/hd` works on node:
 * ------------------------------------------------------
 * - Uncomment the import below and the onRequest method.
 * - Remove Rosalind's hardcoded ID and return the actual value from line 40
 * - Double check if 'cardano-js/hd' export was fixed, so `/dist` can be removed.
 *
 * 👉 If `cardano-js/hd` works on web:
 * ------------------------------------------------------
 * - Delete this IPC implementation
 * - Import `cardano-js` directly on WalletBackupStore.js
 *
 */
// import { Byron, Icarus, newPublicId } from 'cardano-js/dist/hd';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { VERIFY_RECOVERY_PHRASE_CHANNEL } from '../../common/ipc/api';
import type {
  VerifyRecoveryPhraseRendererRequest,
  VerifyRecoveryPhraseMainResponse,
} from '../../common/ipc/api';

const verifyRecoveryPhraseChannel: MainIpcChannel<
  VerifyRecoveryPhraseRendererRequest,
  VerifyRecoveryPhraseMainResponse
> = new MainIpcChannel(VERIFY_RECOVERY_PHRASE_CHANNEL);

export default () => {
  verifyRecoveryPhraseChannel.onRequest(
    ({ recoveryPhrase }: VerifyRecoveryPhraseRendererRequest) => {
      // let xprv;
      // let cc;
      // if (recoveryPhrase.length === 12) {
      //   [xprv, cc] = Byron.generateMasterKey(recoveryPhrase);
      // } else {
      //   [xprv, cc] = Icarus.generateMasterKey(recoveryPhrase);
      // }
      // const walletId = newPublicId(xprv.to_public(), cc);
      // TEMP: Rosalind's wallet ID
      const walletId = 'legacy_aa129a07d1ce083e67597348f1788747a034686e';
      return Promise.resolve(walletId);
    }
  );
};
