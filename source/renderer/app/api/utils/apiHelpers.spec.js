import { throwErrorIfNotEnoughAdaToSupportTokens } from './apiHelpers';

describe('throwErrorIfNotEnoughAdaToSupportTokens', () => {
  it('should not throw if error.code is not "cannot_cover_fee"', () => {
    const error = new Error();
    error.code = 'other_error';
    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).not.toThrow();
  });

  it('should not throw error if error code is "cannot_cover_fee" but message does not match reegex', () => {
    const error = new Error('other message');
    error.code = 'cannot_cover_fee';
    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).not.toThrow();
  });

  it('should not throw error if error code is not "cannot_cover_fee" and message matches regex', () => {
    const error = new Error(
      'I cannot proceed with transaction, I need approximately 1.6 ada to proceed'
    );
    error.code = 'other_code';
    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).not.toThrow();
  });

  it('should not throw if there are no tokens remaining in wallet after transaction', () => {
    const error = new Error();
    error.code = 'cannot_cover_fee';
    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, false)
    ).not.toThrow();
  });

  it('should throw if there are tokens remaining in wallet after transaction and error is "cannot_cover_fee"', () => {
    const error = new Error(
      'I am unable to finalize the transaction, as there is not enough ada available to pay for the fee and also pay for the minimum ada quantities of all change outputs. I need approximately 0.629344 ada to proceed. Try increasing your wallet balance or sending a smaller amount.'
    );
    error.code = 'cannot_cover_fee';

    expect(() =>
      throwErrorIfNotEnoughAdaToSupportTokens(error, true)
    ).toThrowError(
      new Error({
        additionalValues: { adaToRemain: 1 },
        clause: true,
        code: undefined,
        defaultMessage:
          '!!!Insufficient funds to support tokens. A minimum of {adaToRemain} ADA must remain in the wallet after this transaction.',
        forceSet: true,
        id: 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens',
        isFinalError: false,
        tempError: 'cannotLeaveWalletEmpty',
        values: { adaToRemain: 1 },
      })
    );
  });
});
