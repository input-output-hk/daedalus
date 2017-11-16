// @flow
import { generateMnemonic } from '../../utils/crypto';

export type GetEtcAccountRecoveryPhraseResponse = Array<string>;

export const getEtcAccountRecoveryPhrase = (): GetEtcAccountRecoveryPhraseResponse => {
  return generateMnemonic().split(' ');
};
