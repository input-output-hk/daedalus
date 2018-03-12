// @flow
import type { AdaWalletCertificateRecoveryPhraseResponse } from './types';
import { scramblePaperWalletMnemonic } from '../../utils/crypto';

export type GetAdaWalletCertificateRecoveryPhraseParams = {
  passphrase: string,
  input: string,
};

export const getAdaWalletCertificateRecoveryPhrase = (
  { passphrase, input }: GetAdaWalletCertificateRecoveryPhraseParams
): AdaWalletCertificateRecoveryPhraseResponse => (
  scramblePaperWalletMnemonic(passphrase, input)
);
