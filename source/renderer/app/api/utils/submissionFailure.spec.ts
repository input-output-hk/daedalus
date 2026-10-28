import { GenericApiError } from '../common/errors';
import { loadRecordedExchange } from '../../../../../tests/jest/support/walletReplay';
import { definiteSubmissionFailure } from './submissionFailure';

const recordedBody = (backend: 'cip30', name: string) =>
  JSON.parse(loadRecordedExchange(backend, name).response.body);

describe('definiteSubmissionFailure', () => {
  it('names the withdrawal rule when the node refusal is the ledger refusing the withdrawal', () => {
    const error = new GenericApiError(recordedBody('cip30', 'submit-refused'));
    expect(definiteSubmissionFailure(error)).toBe(
      'withdrawal_not_possible_without_vote'
    );
  });

  it('keeps created_invalid_transaction for any other node refusal', () => {
    const error = new GenericApiError({
      code: 'created_invalid_transaction',
      message:
        'The submitted transaction was rejected by the local node. ConwayApplyTxError (ConwayIncompleteWithdrawals)',
    });
    expect(definiteSubmissionFailure(error)).toBe(
      'created_invalid_transaction'
    );
  });

  it('reads the wallet code from an error that wraps the wallet answer in values', () => {
    expect(
      definiteSubmissionFailure({
        values: {
          code: 'created_invalid_transaction',
          message: 'ConwayWdrlNotDelegatedToDRep',
        },
      })
    ).toBe('withdrawal_not_possible_without_vote');
  });

  it('treats the raw dApp backend refusal as definite', () => {
    expect(
      definiteSubmissionFailure(
        recordedBody('cip30', 'dapp-submission-refused')
      )
    ).toBe('dapp_submission_failed');
  });

  it.each([
    ['a socket error', new Error('socket hang up')],
    [
      'an unavailable dApp backend',
      {
        code: 'dapp_submission_unavailable',
        message: 'Transaction submission is unavailable',
      },
    ],
    [
      'a wallet internal error',
      new GenericApiError({ code: 'wallet_internal_error', message: 'x' }),
    ],
    ['nothing', null],
    ['a string', 'failed'],
  ])('leaves the outcome undetermined for %s', (_label, error) => {
    expect(definiteSubmissionFailure(error)).toBeNull();
  });
});
