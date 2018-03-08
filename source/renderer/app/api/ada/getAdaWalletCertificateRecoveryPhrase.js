// @flow
import type { AdaWalletCertificateRecoveryPhraseResponse } from './types';
import { generate15WordMnemonic } from '../../utils/crypto';

// eslint-disable-next-line
export const getAdaWalletCertificateRecoveryPhrase = (): AdaWalletCertificateRecoveryPhraseResponse => (
  generate15WordMnemonic()
);
