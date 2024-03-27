'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateAdditionalMnemonics = exports.generateAccountMnemonics = exports.scrambleMnemonics = exports.unscrambleMnemonics = void 0;
const crypto_1 = require('../../utils/crypto');
const cryptoConfig_1 = require('../../config/cryptoConfig');
const unscrambleMnemonics = ({ passphrase, scrambledInput }) =>
  (0, crypto_1.unscramblePaperWalletMnemonic)(passphrase, scrambledInput);
exports.unscrambleMnemonics = unscrambleMnemonics;
const scrambleMnemonics = ({ passphrase, scrambledInput }) =>
  (0, crypto_1.scramblePaperWalletMnemonic)(passphrase, scrambledInput);
exports.scrambleMnemonics = scrambleMnemonics;
const generateAccountMnemonics = (numberOfWords) =>
  (0, crypto_1.generateMnemonic)(numberOfWords).split(' ');
exports.generateAccountMnemonics = generateAccountMnemonics;
const generateAdditionalMnemonics = () =>
  (0, crypto_1.generateMnemonic)(
    cryptoConfig_1.PAPER_WALLET_WRITTEN_WORDS_COUNT
  ).split(' ');
exports.generateAdditionalMnemonics = generateAdditionalMnemonics;
//# sourceMappingURL=mnemonics.js.map
