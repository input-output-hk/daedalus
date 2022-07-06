import { ErrorType } from '../../domains/ApiError';
import { doesWalletRequireAdaToRemainToSupportTokens } from './apiHelpers';

describe('throw error if not enough Ada to support tokens', () => {
  it('should not throw if error.code is not "cannot_cover_fee"', () => {
    const error: ErrorType = { code: 'bad_request' };

    expect(doesWalletRequireAdaToRemainToSupportTokens(error, true)).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw error if error code is "cannot_cover_fee" but hasAssetsRemainingAfterTransaction is undefined', () => {
    const error: ErrorType = {
      message:
        'I cannot proceed with transaction, I need approximately 1.6 ada to proceed',
      code: 'cannot_cover_fee',
    };

    expect(doesWalletRequireAdaToRemainToSupportTokens(error)).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw error if error code is "cannot_cover_fee" but message does not match regex', () => {
    const error: ErrorType = {
      message: 'other message',
      code: 'cannot_cover_fee',
    };

    expect(doesWalletRequireAdaToRemainToSupportTokens(error, true)).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw error if error code is not "cannot_cover_fee" and message matches regex', () => {
    const error: ErrorType = {
      message:
        'I cannot proceed with transaction, I need approximately 1.6 ada to proceed',
      code: 'bad_request',
    };

    expect(doesWalletRequireAdaToRemainToSupportTokens(error, true)).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });
  it('should not throw if there are no tokens remaining in wallet after transaction', () => {
    const error: ErrorType = { code: 'cannot_cover_fee' };

    expect(doesWalletRequireAdaToRemainToSupportTokens(error, true)).toEqual({
      requiresAdaToRemainToSupportNativeTokens: false,
    });
  });

  it('should throw if there are tokens remaining in wallet after transaction and error is "cannot_cover_fee"', () => {
    const error: ErrorType = {
      message:
        'I am unable to finalize the transaction, as there is not enough ada available to pay for the fee and also pay for the minimum ada quantities of all change outputs. I need approximately 2.629344 ada to proceed. Try increasing your wallet balance or sending a smaller amount.',
      code: 'cannot_cover_fee',
    };

    expect(doesWalletRequireAdaToRemainToSupportTokens(error, true)).toEqual({
      requiresAdaToRemainToSupportNativeTokens: true,
      adaToProceed: 3,
    });
  });
});
