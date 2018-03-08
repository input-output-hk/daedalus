// @flow
import bip39 from 'bip39';
import validWords from '../../../common/valid-words.en';

export const generateMnemonic = () => bip39.generateMnemonic(null, null, validWords);

// TODO - mocked 15-word mnemonic
export const generate15WordMnemonic = () => (
  ['capital', 'cheese', 'cannon', 'easy', 'extend', 'inject', 'identify', 'comfort', 'width', 'tape', 'predict', 'long', 'forest', 'ski', 'require']
);
