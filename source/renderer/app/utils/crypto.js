// @flow
import bip39 from 'bip39';
import { Buffer } from 'safe-buffer';
import { pbkdf2Sync as pbkdf2 } from 'pbkdf2';
import * as unorm from 'unorm';
import CardanoCrypto from 'rust-cardano-crypto';
import validWords from '../../../common/crypto/valid-words.en';

/**
  CS = ENT / 32
  MS = (ENT + CS) / 11

  |  ENT  | CS | ENT+CS |  MS  |
  +-------+----+--------+------+
  |   96  |  3 |    99  |   9  |
  |  128  |  4 |   132  |  12  | (default)
  |  160  |  5 |   165  |  15  |
  |  192  |  6 |   198  |  18  |
  |  224  |  7 |   231  |  21  |
  |  256  |  8 |   264  |  24  |
*/
export const generateMnemonic = (ms: ?number = 15) => {
  let ent;
  switch (ms) {
    case 9:
      ent = 96;
      break;
    case 15:
      ent = 160;
      break;
    case 18:
      ent = 192;
      break;
    case 21:
      ent = 224;
      break;
    case 24:
      ent = 256;
      break;
    default:
      ent = 128;
  }

  return bip39.generateMnemonic(ent, null, validWords);
};

export const scramblePaperWalletMnemonic = (
  passphrase: string,
  input: string
) => {
  const iv = new Uint8Array(8);
  window.crypto.getRandomValues(iv);
  const scrambledInput = CardanoCrypto.PaperWallet.scrambleStrings(
    iv,
    passphrase,
    input
  );
  return scrambledInput.split(' ');
};

export const unscramblePaperWalletMnemonic = (
  passphrase: string,
  scrambledInput: string
) => {
  const input = CardanoCrypto.PaperWallet.unscrambleStrings(
    passphrase,
    scrambledInput
  );
  return input.split(' ');
};

export const mnemonicToSeedHex = (mnemonic: string, password: ?string) => {
  const mnemonicBuffer = Buffer.from(unorm.nfkd(mnemonic), 'utf8');
  const salt = `mnemonic${unorm.nfkd(password) || ''}`;
  const saltBuffer = Buffer.from(salt, 'utf8');
  return pbkdf2(mnemonicBuffer, saltBuffer, 2048, 32, 'sha512').toString('hex');
};
