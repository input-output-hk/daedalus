// @flow
import type { AdaWalletRecoveryPhraseResponse } from './types';
import { generateMnemonic } from '../../utils/crypto';

export const getAdaAccountRecoveryPhrase = (): AdaWalletRecoveryPhraseResponse => (
  generateMnemonic().split(' ')
);
