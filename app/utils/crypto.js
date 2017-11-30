// @flow
import bip39 from 'bip39';
import validWords from '../../lib/valid-words.en';

export const generateMnemonic = () => bip39.generateMnemonic(null, null, validWords);
