// @flow
import bip39 from 'bip39';
import CardanoCrypto from 'rust-cardano-crypto';
import validWords from '../../../common/valid-words.en';

export const generateMnemonic = () => bip39.generateMnemonic(null, null, validWords);

export const scramblePaperWalletMnemonic = (
  passphrase: string, input: string
) => {
  const iv = new Uint8Array(4);
  window.crypto.getRandomValues(iv);
  const scrambledInput = CardanoCrypto.PaperWallet.scrambleStrings(iv, passphrase, input);
  return scrambledInput.split(' ');
};

export const unscramblePaperWalletMnemonic = (
  passphrase: string, scrambledInput: string
) => {
  const input = CardanoCrypto.PaperWallet.unscrambleStrings(passphrase, scrambledInput);
  return input.split(' ');
};
