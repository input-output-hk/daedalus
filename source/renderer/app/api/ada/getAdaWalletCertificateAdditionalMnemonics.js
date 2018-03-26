// @flow
import type { AdaWalletCertificateAdditionalMnemonicsResponse } from './types';
import { generateMnemonic } from '../../utils/crypto';

// eslint-disable-next-line
export const getAdaWalletCertificateAdditionalMnemonics = (): AdaWalletCertificateAdditionalMnemonicsResponse => (
  generateMnemonic(9).split(' ')
);
