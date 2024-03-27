'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const apiHelpers_1 = require('./apiHelpers');
describe('throw error if not enough Ada to support tokens', () => {
  it('should not throw if error.code is not "cannot_cover_fee"', () => {
    const error = { code: 'bad_request' };
    expect(
      (0, apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(error, true)
    ).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw error if error code is "cannot_cover_fee" but hasAssetsRemainingAfterTransaction is undefined', () => {
    const error = {
      message:
        'I cannot proceed with transaction, I need approximately 1.6 ada to proceed',
      code: 'cannot_cover_fee',
    };
    expect(
      (0, apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(error)
    ).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw error if error code is "cannot_cover_fee" but message does not match regex', () => {
    const error = {
      message: 'other message',
      code: 'cannot_cover_fee',
    };
    expect(
      (0, apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(error, true)
    ).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw error if error code is not "cannot_cover_fee" and message matches regex', () => {
    const error = {
      message:
        'I cannot proceed with transaction, I need approximately 1.6 ada to proceed',
      code: 'bad_request',
    };
    expect(
      (0, apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(error, true)
    ).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw if there are no tokens remaining in wallet after transaction', () => {
    const error = { code: 'cannot_cover_fee' };
    expect(
      (0, apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(error, true)
    ).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should throw if there are tokens remaining in wallet after transaction and error is "cannot_cover_fee"', () => {
    const error = {
      message:
        'I am unable to finalize the transaction, as there is not enough ada available to pay for the fee and also pay for the minimum ada quantities of all change outputs. I need approximately 2.629344 ada to proceed. Try increasing your wallet balance or sending a smaller amount.',
      code: 'cannot_cover_fee',
    };
    expect(
      (0, apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(error, true)
    ).toEqual({
      requiresAdaToRemainToSupportNativeTokens: true,
      adaToProceed: 3,
    });
  });
});
//# sourceMappingURL=apiHelpers.spec.js.map
