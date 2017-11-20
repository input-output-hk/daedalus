// @flow
import { generateMnemonic } from '../../utils/crypto';

export type GetAdaAccountRecoveryPhraseResponse = Array<string>;

export const getAdaAccountRecoveryPhrase = (): GetAdaAccountRecoveryPhraseResponse => (
  generateMnemonic().split(' ')
);
