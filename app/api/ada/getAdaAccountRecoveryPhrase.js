// @flow
import { generateMnemonic } from '../../utils/crypto';

export type ApiWalletRecoveryPhraseResponse = Array<string>;

export const getAdaAccountRecoveryPhrase = (): ApiWalletRecoveryPhraseResponse => (
  generateMnemonic().split(' ')
);
