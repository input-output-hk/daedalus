// @flow
import bip39 from 'bip39';
import { Buffer } from 'safe-buffer';
import crypto from 'crypto';
import { chunk } from 'lodash';
import { pbkdf2Sync as pbkdf2 } from 'pbkdf2';
import * as unorm from 'unorm';
import CardanoCrypto from 'rust-cardano-crypto';
import validWords from '../../../common/crypto/valid-words.en';
import { ADA_CERTIFICATE_MNEMONIC_LENGTH } from '../config/cryptoConfig';

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
  console.debug('WORDS: ', ms)
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
  let iv;
  if (typeof window !== 'undefined') {
    iv = new Uint8Array(8);
    window.crypto.getRandomValues(iv);
  } else {
    // Window is not defined for UNIT test
    iv = crypto.randomBytes(8).toJSON().data;
  }

  const scrambledInput = CardanoCrypto.PaperWallet.scrambleStrings(
    iv,
    passphrase,
    input
  );
  return scrambledInput.split(' ');
};

export const getScrambledInput = (mnemonics: Array<string>) => {
  const chunked = chunk(mnemonics, ADA_CERTIFICATE_MNEMONIC_LENGTH);
  const scrambledInput = chunked[0].join(' '); // first 18 mnemonics
  const certificatePassword = chunked[1]; // last 9 mnemonics
  const passphrase = mnemonicToSeedHex(certificatePassword.join(' '));
  return { passphrase, scrambledInput };
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
