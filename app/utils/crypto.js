// @flow
import validWords from '../../lib/valid-words.en';
import bip39 from 'bip39';

export const generateMnemonic = () => bip39.generateMnemonic(null, null, validWords);
