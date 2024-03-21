'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.decryptRecoveryForceVend = exports.decryptRecoveryRegularVend = exports.decryptForceVend = exports.decryptRegularVend = exports.isValidMnemonic = void 0;
const aes_js_1 = __importDefault(require('aes-js'));
const bip39 = __importStar(require('bip39'));
const blakejs_1 = __importDefault(require('blakejs'));
const crypto_1 = __importDefault(require('crypto'));
const valid_words_en_1 = __importDefault(require('./valid-words.en'));
const iv = Buffer.alloc(16); // it's iv = 0 simply
function decryptWithAES(aesKey, bytes) {
  // eslint-disable-next-line new-cap
  return new aes_js_1.default.ModeOfOperation.ctr(
    aesKey,
    new aes_js_1.default.Counter(iv)
  ).decrypt(bytes);
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
const blake2b = (data) => blakejs_1.default.blake2b(data, null, 32);
const fromMnemonic = (words) =>
  hexToBytes(bip39.mnemonicToEntropy(words, valid_words_en_1.default));
const isValidMnemonic = (phrase, numberOfWords = 9) =>
  phrase.split(' ').length === numberOfWords &&
  bip39.validateMnemonic(phrase, valid_words_en_1.default);
exports.isValidMnemonic = isValidMnemonic;
const hashData = (data) => {
  const hash = crypto_1.default.createHash('sha256');
  hash.update(data, 'utf8');
  return hash.digest();
};
const decryptRegularVend = (key, data) =>
  decryptWithAES(blake2b(fromMnemonic(key)), data);
exports.decryptRegularVend = decryptRegularVend;
const decryptForceVend = (key, data) =>
  decryptWithAES(
    blake2b(
      key[0].trim().toLowerCase() +
        // @ts-ignore ts-migrate(2339) FIXME: Property 'hexSlice' does not exist on type 'Buffer... Remove this comment to see the full error message
        hashData(key[1].trim()).hexSlice() +
        key[2].trim()
    ),
    data
  );
exports.decryptForceVend = decryptForceVend;
// Recovery service certificates decryption
exports.decryptRecoveryRegularVend = exports.decryptRegularVend;
const decryptRecoveryForceVend = (key, data) => {
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
exports.decryptRecoveryForceVend = decryptRecoveryForceVend;
//# sourceMappingURL=decrypt.js.map
