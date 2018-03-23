// @flow
import type { AdaWalletCertificateRecoveryPhraseResponse } from './types';
import { scramblePaperWalletMnemonic } from '../../utils/crypto';

export type GetAdaWalletCertificateRecoveryPhraseParams = {
  passphrase: string,
  scrambledInput: string,
};

export const getAdaWalletCertificateRecoveryPhrase = (
  { passphrase, scrambledInput }: GetAdaWalletCertificateRecoveryPhraseParams
): AdaWalletCertificateRecoveryPhraseResponse => (
  scramblePaperWalletMnemonic(passphrase, scrambledInput)
);
