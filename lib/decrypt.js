import aesjs from 'aes-js';
import bip39 from 'bip39';
import blakejs from 'blakejs';
import crypto from 'crypto';
import validWords from './valid-words.en';

const iv = new Buffer(16); // it's iv = 0 simply

function decryptWithAES(aesKey, bytes) {
  return new aesjs.ModeOfOperation.ctr(aesKey, new aesjs.Counter(iv)).decrypt(bytes); // eslint-disable-line
}

const hexChar = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

const hexToBytes = (s) => {
  const arr = [];
  if (s.length & 1 === 1) { // eslint-disable-line
    throw new Error(`Wrong hex: ${s}`);
  }
  for (let i = 0; i < s.length / 2; ++i) {
    const c1 = s[2 * i];
    const c2 = s[(2 * i) + 1];
    const i1 = hexChar.indexOf(c1);
    const i2 = hexChar.indexOf(c2);
    if (i1 === -1 || i2 === -1) throw new Error(`Wrong hex: ${s}`);
    arr[i] = (i1 << 4) + i2;
  }
  return new Uint8Array(arr);
};

const blake2b = (data) => blakejs.blake2b(data, null, 32);

const fromMnemonic = (words) => hexToBytes(bip39.mnemonicToEntropy(words, validWords));

export const isValidMnemonic = (phrase, numberOfWords = 9) => (
  (phrase.split(' ').length === numberOfWords && bip39.validateMnemonic(phrase, validWords))
);

const hashData = (data) => {
  const hash = crypto.createHash('sha256');
  hash.update(data, 'utf8');
  return hash.digest();
};

export const decryptRegularVend = (key, data) => decryptWithAES(blake2b(fromMnemonic(key)), data);
export const decryptForceVend = (key, data) => decryptWithAES(blake2b(key[0].trim().toLowerCase() + hashData(key[1].trim()).hexSlice() + key[2].trim()), data);
