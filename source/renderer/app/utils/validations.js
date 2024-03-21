'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.isValidRepeatPinCode = exports.isValidPinCode = exports.validateMnemonics = exports.INCOMPLETE_MNEMONIC_MARKER = exports.isValidAssetAmountInNaturalUnits = exports.isValidAmountInLovelaces = exports.isNotEmptyString = exports.isValidRepeatPassword = exports.isValidSpendingPassword = exports.isUnicaseString = exports.isCaselessString = exports.containsUnicaseLetter = exports.containsUpperCaseLetter = exports.containsLowerCaseLetter = exports.containsDecimalNumber = exports.containsWhitespace = exports.isValidWalletName = void 0;
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const isInt_1 = __importDefault(require('validator/lib/isInt'));
const lodash_1 = require('lodash');
const MIN_PASSWORD_LENGTH = 10;
const MAX_PASSWORD_LENGTH = 255;
const isValidWalletName = (walletName) => {
  const nameLength = walletName.length;
  return nameLength >= 3 && nameLength <= 40;
};
exports.isValidWalletName = isValidWalletName;
/**
 * Checks if a string contains unicode white space
 */
const containsWhitespace = (s) => /\p{White_Space}/u.test(s);
exports.containsWhitespace = containsWhitespace;
/**
 * Checks if a string contains a decimal number
 */
const containsDecimalNumber = (s) => /\p{Decimal_Number}/u.test(s);
exports.containsDecimalNumber = containsDecimalNumber;
/**
 * Checks if a string contains a lower case letter
 */
const containsLowerCaseLetter = (s) => /\p{Lowercase_Letter}/u.test(s);
exports.containsLowerCaseLetter = containsLowerCaseLetter;
/**
 * Checks if a string contains a upper case letter
 */
const containsUpperCaseLetter = (s) => /\p{Uppercase_Letter}/u.test(s);
exports.containsUpperCaseLetter = containsUpperCaseLetter;
/**
 * Checks if a string contains a unicase letter
 * (E.g: Languages like Kanji do not have the concept of letter case)
 */
const containsUnicaseLetter = (s) => /\p{Other_Letter}/u.test(s);
exports.containsUnicaseLetter = containsUnicaseLetter;
/**
 * Checks if a string doesn't change after upper and lower casing it
 */
const isCaselessString = (s) => s.toLowerCase() === s && s.toUpperCase() === s;
exports.isCaselessString = isCaselessString;
/**
 * Test if a whole string is in unicase letters (or digits)
 */
const isUnicaseString = (
  password // We require at least one unicase letter
) =>
  (0, exports.containsUnicaseLetter)(password) && // Every char has to belong to the support caseless categories
  (0, lodash_1.every)(password.split(''), (char) =>
    (0, exports.isCaselessString)(char)
  );
exports.isUnicaseString = isUnicaseString;
/**
 * Enforces passwords without spaces and a minimum of 10 characters and a maximum of 255 characters.
 */
const isValidSpendingPassword = (password) => {
  // Should contain at least 10 characters
  return (
    password.length >= MIN_PASSWORD_LENGTH &&
    password.length <= MAX_PASSWORD_LENGTH
  );
};
exports.isValidSpendingPassword = isValidSpendingPassword;
// eslint-disable-next-line max-len
const isValidRepeatPassword = (spendingPassword, repeatPassword) =>
  spendingPassword === repeatPassword;
exports.isValidRepeatPassword = isValidRepeatPassword;
const isNotEmptyString = (value) => value !== '';
exports.isNotEmptyString = isNotEmptyString;
const isValidAmountInLovelaces = (value) => {
  const isNumeric = (0, isInt_1.default)(value, {
    allow_leading_zeroes: false,
  });
  if (!isNumeric) return false;
  const numericValue = new bignumber_js_1.default(value);
  const minValue = new bignumber_js_1.default(1);
  const maxValue = new bignumber_js_1.default(45000000000000000);
  return numericValue.gte(minValue) && numericValue.lte(maxValue);
};
exports.isValidAmountInLovelaces = isValidAmountInLovelaces;
const isValidAssetAmountInNaturalUnits = (value) => {
  const isNumeric = (0, isInt_1.default)(value, {
    allow_leading_zeroes: false,
  });
  if (!isNumeric) return false;
  const numericValue = new bignumber_js_1.default(value);
  const minValue = new bignumber_js_1.default(1);
  const maxValue = new bignumber_js_1.default('18446744073709551615'); // cardano-wallet max asset amount of 2^64 - 1
  return numericValue.gte(minValue) && numericValue.lte(maxValue);
};
exports.isValidAssetAmountInNaturalUnits = isValidAssetAmountInNaturalUnits;
exports.INCOMPLETE_MNEMONIC_MARKER = 'INCOMPLETE_MNEMONIC_MARKER';
function validateMnemonics(params) {
  const { requiredWords, providedWords } = params;
  const providedWordsCount = providedWords.length;
  const isPhraseComplete = Array.isArray(requiredWords)
    ? requiredWords.includes(providedWordsCount)
    : providedWordsCount === requiredWords;
  if (!isPhraseComplete) {
    return exports.INCOMPLETE_MNEMONIC_MARKER;
  }
  return params.validator(providedWords);
}
exports.validateMnemonics = validateMnemonics;
/**
 * Voting PIN code validation
 */
const isValidPinCode = (pinCode, length) => {
  return pinCode.length === length;
};
exports.isValidPinCode = isValidPinCode;
const isValidRepeatPinCode = (pinCode, repeatPinCode) =>
  pinCode === repeatPinCode;
exports.isValidRepeatPinCode = isValidRepeatPinCode;
//# sourceMappingURL=validations.js.map
