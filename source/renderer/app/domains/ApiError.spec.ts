import { knownDefect } from '../../../../tests/jest/support/knownDefect';
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
      .set('conwayWalletNotDelegatedToDRep')
      .where('code', 'created_invalid_transaction')
      .inc('message', 'ConwayWdrlNotDelegatedToDRep')
      .result();
    expect(error.id).toBe('api.errors.conwayWalletNotDelegatedToDRep');
  });

  it('keeps a matched clause when one further clause follows', () => {
    const error = new ApiError(refusal)
      .set('conwayWalletNotDelegatedToDRep')
      .where('code', 'created_invalid_transaction')
      .set('transactionIsTooBig', true)
      .where('code', 'transaction_is_too_big')
      .result();
    expect(error.id).toBe('api.errors.conwayWalletNotDelegatedToDRep');
  });

  knownDefect(
    'keeps a matched clause when two further clauses follow',
    'set() after a match only closes the clause, so the next set() replaces the match and its failed where() clears it',
    () => {
      const error = new ApiError(refusal)
        .set('conwayWalletNotDelegatedToDRep')
        .where('code', 'created_invalid_transaction')
        .set('conwayWalletNotDelegatedToDRep')
        .where('code', 'withdrawal_not_possible_without_vote')
        .set('transactionIsTooBig', true)
        .where('code', 'transaction_is_too_big')
        .result();
      expect(error.id).toBe('api.errors.conwayWalletNotDelegatedToDRep');
    }
  );
});
