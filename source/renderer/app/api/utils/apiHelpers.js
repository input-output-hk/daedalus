'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.doesWalletRequireAdaToRemainToSupportTokens = exports.wait = exports.testSync = exports.testAsync = exports.notYetImplemented = void 0;
const errors_1 = require('../common/errors');
const notYetImplemented = async () =>
  new Promise((resolve, reject) => {
    reject(new errors_1.ApiMethodNotYetImplementedError());
  });
exports.notYetImplemented = notYetImplemented;
// helper code for testing async APIs
const testAsync = async (apiMethod) => {
  const result = await apiMethod();
  // eslint-disable-next-line no-console
  console.log(`testAsync result: ${result}`);
  return result;
};
exports.testAsync = testAsync;
// helper code for testing sync APIs
const testSync = (apiMethod) => {
  const result = apiMethod();
  // eslint-disable-next-line no-console
  console.log(`testSync result: ${result}`);
  return result;
};
exports.testSync = testSync;
// helper code for deferring API call execution
const wait = (ms) =>
  new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
exports.wait = wait;
const doesWalletRequireAdaToRemainToSupportTokens = (
  error,
  hasAssetsRemainingAfterTransaction
) => {
  const adaToProceedRegex = /I need approximately\s+([\d.,]+)\s+ada to proceed/;
  const [, adaToProceed] = adaToProceedRegex.exec(error.message) ?? [];
  if (
    error.code === 'cannot_cover_fee' &&
    hasAssetsRemainingAfterTransaction &&
    adaToProceed
  ) {
    return {
      requiresAdaToRemainToSupportNativeTokens: true,
      adaToProceed: Math.ceil(Number(adaToProceed)),
    };
  }
  return { requiresAdaToRemainToSupportNativeTokens: false };
};
exports.doesWalletRequireAdaToRemainToSupportTokens = doesWalletRequireAdaToRemainToSupportTokens;
//# sourceMappingURL=apiHelpers.js.map
