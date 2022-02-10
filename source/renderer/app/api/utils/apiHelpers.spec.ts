import { throwErrorIfNotEnoughAdaToSupportTokens } from './apiHelpers';
import ApiError from '../../domains/ApiError';

describe('throwErrorIfNotEnoughAdaToSupportTokens', () => {
  it('should not throw if error.code is not "cannot_cover_fee"', () => {
    const error = { code: 'other_error' };

    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).not.toThrow();
  });
  it('should not throw error if error code is "cannot_cover_fee" but hasAssetsRemainingAfterTransaction is undefined', () => {
    const error = {
      message:
        'I cannot proceed with transaction, I need approximately 1.6 ada to proceed',
      code: 'cannot_cover_fee',
    };

    expect(() => throwErrorIfNotEnoughAdaToSupportTokens(error)).not.toThrow();
  });
  it('should not throw error if error code is "cannot_cover_fee" but message does not match reegex', () => {
    const error = {
      message: 'other message',
      code: 'cannot_cover_fee',
    };

    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).not.toThrow();
  });
  it('should not throw error if error code is not "cannot_cover_fee" and message matches regex', () => {
    const error = {
      message:
        'I cannot proceed with transaction, I need approximately 1.6 ada to proceed',
      code: 'other_code',
    };

    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).not.toThrow();
  });
  it('should not throw if there are no tokens remaining in wallet after transaction', () => {
    const error = { code: 'cannot_cover_fee' };

    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, false)
    ).not.toThrow();
  });
  it('should throw if there are tokens remaining in wallet after transaction and error is "cannot_cover_fee" and round to 2 minimum ada', () => {
    const error = {
      message:
        'I am unable to finalize the transaction, as there is not enough ada available to pay for the fee and also pay for the minimum ada quantities of all change outputs. I need approximately 0.629344 ada to proceed. Try increasing your wallet balance or sending a smaller amount.',
      code: 'cannot_cover_fee',
    };

    expect(() => throwErrorIfNotEnoughAdaToSupportTokens(error, true)).toThrow(
      expect.objectContaining({
        additionalValues: { adaToRemain: 2 },
        clause: true,
        code: undefined,
        defaultMessage:
          '!!!Insufficient funds to support tokens. A minimum of {adaToRemain} ADA must remain in the wallet after this transaction.',
        forceSet: true,
        id: 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens',
        isFinalError: false,
        tempError: 'cannotLeaveWalletEmpty',
        values: { adaToRemain: 2 },
      })
    );
  });

  it('should throw if there are tokens remaining in wallet after transaction and error is "cannot_cover_fee" and round to 2 nearest whole value provided by error', () => {
    const error = {
      message:
        'I am unable to finalize the transaction, as there is not enough ada available to pay for the fee and also pay for the minimum ada quantities of all change outputs. I need approximately 2.629344 ada to proceed. Try increasing your wallet balance or sending a smaller amount.',
      code: 'cannot_cover_fee',
    };

    expect(() => throwErrorIfNotEnoughAdaToSupportTokens(error, true)).toThrow(
      expect.objectContaining({
        additionalValues: { adaToRemain: 3 },
        clause: true,
        code: undefined,
        defaultMessage:
          '!!!Insufficient funds to support tokens. A minimum of {adaToRemain} ADA must remain in the wallet after this transaction.',
        forceSet: true,
        id: 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens',
        isFinalError: false,
        tempError: 'cannotLeaveWalletEmpty',
        values: { adaToRemain: 3 },
      })
    );
  });
});
