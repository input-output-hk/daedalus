// @flow
import type { AdaWalletRecoveryPhraseFromCertificateResponse } from './types';
import { unscramblePaperWalletMnemonic } from '../../utils/crypto';

export type GetAdaWalletRecoveryPhraseFromCertificateParams = {
  passphrase: string, // 9-word mnemonic
  scrambledInput: string, // 15-word scrambled mnemonic
};

export const getAdaWalletRecoveryPhraseFromCertificate = (
  { passphrase, scrambledInput }: GetAdaWalletRecoveryPhraseFromCertificateParams
): AdaWalletRecoveryPhraseFromCertificateResponse => (
  unscramblePaperWalletMnemonic(passphrase, scrambledInput)
);
