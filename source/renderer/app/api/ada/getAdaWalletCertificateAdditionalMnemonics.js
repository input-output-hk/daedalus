// @flow
import type { AdaWalletCertificateAdditionalMnemonicsResponse } from './types';
import { generateMnemonic } from '../../utils/crypto';
import { PAPER_WALLET_WRITTEN_WORDS_COUNT } from '../../config/cryptoConfig';

// eslint-disable-next-line
export const getAdaWalletCertificateAdditionalMnemonics = (): AdaWalletCertificateAdditionalMnemonicsResponse => (
  generateMnemonic(PAPER_WALLET_WRITTEN_WORDS_COUNT).split(' ')
);
