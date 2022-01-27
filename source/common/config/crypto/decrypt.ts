import aesjs from 'aes-js';
import * as bip39 from 'bip39';
import blakejs from 'blakejs';
import crypto from 'crypto';
import validWords from './valid-words.en';

const iv = Buffer.alloc(16); // it's iv = 0 simply

function decryptWithAES(aesKey, bytes) {
  // eslint-disable-next-line new-cap
  return new aesjs.ModeOfOperation.ctr(aesKey, new aesjs.Counter(iv)).decrypt(
    bytes
  );
}

const hexChar = [
  '0',
  '1',
  '2',
  '3',
  '4',
  '5',
  '6',
  '7',
  '8',
  '9',
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
];

const hexToBytes = (s) => {
  const arr = [];

  // @ts-ignore ts-migrate(2363) FIXME: The right-hand side of an arithmetic operation mus... Remove this comment to see the full error message
  // eslint-disable-next-line no-self-compare
  if (s.length & (1 === 1)) {
    throw new Error(`Wrong hex: ${s}`);
  }

  for (let i = 0; i < s.length / 2; ++i) {
    const c1 = s[2 * i];
    const c2 = s[2 * i + 1];
    const i1 = hexChar.indexOf(c1);
    const i2 = hexChar.indexOf(c2);
    if (i1 === -1 || i2 === -1) throw new Error(`Wrong hex: ${s}`);
    arr[i] = (i1 << 4) + i2;
  }

  return new Uint8Array(arr);
};

const blake2b = (data) => blakejs.blake2b(data, null, 32);

const fromMnemonic = (words) =>
  hexToBytes(bip39.mnemonicToEntropy(words, validWords));

export const isValidMnemonic = (phrase, numberOfWords = 9) =>
  phrase.split(' ').length === numberOfWords &&
  bip39.validateMnemonic(phrase, validWords);

const hashData = (data) => {
  const hash = crypto.createHash('sha256');
  hash.update(data, 'utf8');
  return hash.digest();
};

export const decryptRegularVend = (key, data) =>
  decryptWithAES(blake2b(fromMnemonic(key)), data);
export const decryptForceVend = (key, data) =>
  decryptWithAES(
    blake2b(
      key[0].trim().toLowerCase() +
        // @ts-ignore ts-migrate(2339) FIXME: Property 'hexSlice' does not exist on type 'Buffer... Remove this comment to see the full error message
        hashData(key[1].trim()).hexSlice() +
        key[2].trim()
    ),
    data
  );
// Recovery service certificates decryption
export const decryptRecoveryRegularVend = decryptRegularVend;
export const decryptRecoveryForceVend = (key, data) => {
  // There are 3 possible decryption key formats:
  // 1) base64 string (most common)
  // 2) hex string
  // 3) numeric array
  // ...therefore we need to try all 3 decryption methods
  const trimmedKey = key.trim();
  let decryptedData = null;
  let bufferKey;

  // 1) base64 string: "qXQWDxI3JrlFRtC4SeQjeGzLbVXWBomYPbNO1Vfm1T4="
  try {
    const decodedKey = trimmedKey.replace(/-/g, '+').replace(/_/g, '/');
    bufferKey = Buffer.from(decodedKey, 'base64');
    decryptedData = decryptWithAES(bufferKey, data);
  } catch (e) {}

  // eslint-disable-line
  // 2) hex string: "A974160F123726B94546D0B849E423786CCB6D55D60689983DB34ED557E6D53E"
  if (decryptedData === null) {
    try {
      bufferKey = Buffer.from(trimmedKey, 'hex');
      decryptedData = decryptWithAES(bufferKey, data);
    } catch (e) {} // eslint-disable-line
  }

  // eslint-disable-next-line max-len
  // 3) numeric array: "[ 169, 116, 22, 15, 18, 55, 38, 185, 69, 70, 208, 184, 73, 228, 35, 120, 108, 203, 109, 85, 214, 6, 137, 152, 61, 179, 78, 213, 87, 230, 213, 62 ]"
  if (decryptedData === null) {
    try {
      const arrayKey = JSON.parse(trimmedKey);
      bufferKey = Buffer.from(arrayKey);
      decryptedData = decryptWithAES(bufferKey, data);
    } catch (e) {} // eslint-disable-line
  }

  if (decryptedData === null) {
    throw new Error('Invalid decryption key');
  } else {
    return decryptedData;
  }
};
