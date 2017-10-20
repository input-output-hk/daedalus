// @flow
import bip39 from 'bip39';
import validWords from '../../../lib/valid-words.en';

export type GetEtcAccountRecoveryPhraseResponse = string[];

export const getEtcAccountRecoveryPhrase = (): GetEtcAccountRecoveryPhraseResponse => {
  const mnemonics = bip39.generateMnemonic(null, null, validWords);
  return mnemonics.split(' ');
};
