import ApiError from './ApiError';

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

// A node refusal as cardano-wallet reports it. `created_invalid_transaction`
// has no message of its own, so the error starts out unresolved.
const refusal = {
  code: 'created_invalid_transaction',
  message:
    'The submitted transaction was rejected by the local node. ConwayApplyTxError (ConwayWdrlNotDelegatedToDRep (KeyHash {unKeyHash = "00"} :| []))',
} as const;

describe('ApiError', () => {
  it('resolves to the message of the clause that matches', () => {
    const error = new ApiError(refusal)
      .set('withdrawalNotPossibleWithoutVote')
      .where('code', 'created_invalid_transaction')
      .inc('message', 'ConwayWdrlNotDelegatedToDRep')
      .result();
    expect(error.id).toBe('api.errors.withdrawalNotPossibleWithoutVote');
  });

  it('keeps a matched clause when one further clause follows', () => {
    const error = new ApiError(refusal)
      .set('withdrawalNotPossibleWithoutVote')
      .where('code', 'created_invalid_transaction')
      .set('transactionIsTooBig', true)
      .where('code', 'transaction_is_too_big')
      .result();
    expect(error.id).toBe('api.errors.withdrawalNotPossibleWithoutVote');
  });

  it('keeps a matched clause when three further clauses follow', () => {
    const error = new ApiError(refusal)
      .set('withdrawalNotPossibleWithoutVote')
      .where('code', 'created_invalid_transaction')
      .set('withdrawalNotPossibleWithoutVote')
      .where('code', 'withdrawal_not_possible_without_vote')
      .set('transactionIsTooBig', true)
      .where('code', 'transaction_is_too_big')
      .set('wrongEncryptionPassphrase')
      .where('code', 'bad_request')
      .inc('message', 'passphrase is too short')
      .result();
    expect(error.id).toBe('api.errors.withdrawalNotPossibleWithoutVote');
  });

  it('lets the next clause match after a clause fails', () => {
    const error = new ApiError({
      code: 'bad_request',
      message: 'passphrase is too short',
    })
      .set('withdrawalNotPossibleWithoutVote')
      .where('code', 'created_invalid_transaction')
      .inc('message', 'ConwayWdrlNotDelegatedToDRep')
      .set('transactionIsTooBig', true)
      .where('code', 'transaction_is_too_big')
      .set('wrongEncryptionPassphrase')
      .where('code', 'bad_request')
      .inc('message', 'passphrase is too short')
      .result();
    expect(error.id).toBe('api.errors.IncorrectPasswordError');
  });

  it('resolves withdrawal_not_possible_without_vote to its message without a clause', () => {
    const error = new ApiError({
      code: 'withdrawal_not_possible_without_vote',
      message: 'The ledger holds no vote delegation.',
    }).result();
    expect(error.id).toBe('api.errors.withdrawalNotPossibleWithoutVote');
  });
});
