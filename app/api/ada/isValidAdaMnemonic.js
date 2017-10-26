import bip39 from 'bip39';
import validWords from '../../../lib/valid-words.en';

export const isValidAdaMnemonic = (phrase, numberOfWords = 9) => (
  (phrase.split(' ').length === numberOfWords && bip39.validateMnemonic(phrase, validWords))
);
