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
exports.getStakeAddressFromStakeKey = exports.encodeBech32 = exports.decodeBech32 = exports.blake2b224 = exports.mnemonicToSeedHex = exports.unscramblePaperWalletMnemonic = exports.getScrambledInput = exports.scramblePaperWalletMnemonic = exports.generateMnemonic = void 0;
const bip39 = __importStar(require('bip39'));
const safe_buffer_1 = require('safe-buffer');
const blakejs_1 = require('blakejs');
const bech32_1 = require('bech32');
const crypto_1 = __importDefault(require('crypto'));
const lodash_1 = require('lodash');
const pbkdf2_1 = require('pbkdf2');
const unorm = __importStar(require('unorm'));
const rust_cardano_crypto_1 = __importDefault(require('rust-cardano-crypto'));
const valid_words_en_1 = __importDefault(
  require('../../../common/config/crypto/valid-words.en')
);
const cryptoConfig_1 = require('../config/cryptoConfig');
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
const generateMnemonic = (ms = 15) => {
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
  return bip39.generateMnemonic(ent, null, valid_words_en_1.default);
};
exports.generateMnemonic = generateMnemonic;
const scramblePaperWalletMnemonic = (passphrase, input) => {
  let iv;
  if (typeof window !== 'undefined') {
    iv = new Uint8Array(8);
    window.crypto.getRandomValues(iv);
  } else {
    // Window is not defined for UNIT test
    iv = crypto_1.default.randomBytes(8).toJSON().data;
  }
  const scrambledInput = rust_cardano_crypto_1.default.PaperWallet.scrambleStrings(
    iv,
    passphrase,
    input
  );
  return scrambledInput.split(' ');
};
exports.scramblePaperWalletMnemonic = scramblePaperWalletMnemonic;
const getScrambledInput = (mnemonics) => {
  const chunked = (0, lodash_1.chunk)(
    mnemonics,
    cryptoConfig_1.ADA_CERTIFICATE_MNEMONIC_LENGTH
  );
  const scrambledInput = chunked[0].join(' '); // first 18 mnemonics
  const certificatePassword = chunked[1]; // last 9 mnemonics
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  const passphrase = (0, exports.mnemonicToSeedHex)(
    certificatePassword.join(' ')
  );
  return {
    passphrase,
    scrambledInput,
  };
};
exports.getScrambledInput = getScrambledInput;
const unscramblePaperWalletMnemonic = (passphrase, scrambledInput) => {
  const input = rust_cardano_crypto_1.default.PaperWallet.unscrambleStrings(
    passphrase,
    scrambledInput
  );
  return input.split(' ');
};
exports.unscramblePaperWalletMnemonic = unscramblePaperWalletMnemonic;
const mnemonicToSeedHex = (mnemonic, password) => {
  const mnemonicBuffer = safe_buffer_1.Buffer.from(
    unorm.nfkd(mnemonic),
    'utf8'
  );
  const salt = `mnemonic${unorm.nfkd(password) || ''}`;
  const saltBuffer = safe_buffer_1.Buffer.from(salt, 'utf8');
  return (0, pbkdf2_1.pbkdf2Sync)(
    mnemonicBuffer,
    saltBuffer,
    2048,
    32,
    'sha512'
  ).toString('hex');
};
exports.mnemonicToSeedHex = mnemonicToSeedHex;
const blake2b224 = (data) => (0, blakejs_1.blake2b)(data, null, 28);
exports.blake2b224 = blake2b224;
const decodeBech32 = (data) =>
  safe_buffer_1.Buffer.from(
    bech32_1.bech32.fromWords(bech32_1.bech32.decode(data).words)
  );
exports.decodeBech32 = decodeBech32;
const encodeBech32 = (prefix, data) =>
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Buffer' is not assignable to par... Remove this comment to see the full error message
  bech32_1.bech32.encode(prefix, bech32_1.bech32.toWords(data));
exports.encodeBech32 = encodeBech32;
const getStakeAddressFromStakeKey = (stakeKey) => {
  const { isMainnet, isStaging, isSelfnode } = global.environment;
  const isMainnetLikeNetwork = isMainnet || isStaging || isSelfnode;
  const stakeKeyHex = (0, exports.decodeBech32)(stakeKey);
  const stakeKeyHash = (0, exports.blake2b224)(stakeKeyHex);
  const networkPrefix = safe_buffer_1.Buffer.from(
    isMainnetLikeNetwork ? 'e1' : 'e0',
    'hex'
  );
  const addressPrefix = isMainnetLikeNetwork ? 'stake' : 'stake_test';
  const stakeAddress = (0, exports.encodeBech32)(
    addressPrefix,
    // @ts-ignore ts-migrate(2488) FIXME: Type 'Buffer' must have a '[Symbol.iterator]()' me... Remove this comment to see the full error message
    safe_buffer_1.Buffer.from([...networkPrefix, ...stakeKeyHash])
  );
  return stakeAddress;
};
exports.getStakeAddressFromStakeKey = getStakeAddressFromStakeKey;
//# sourceMappingURL=crypto.js.map
