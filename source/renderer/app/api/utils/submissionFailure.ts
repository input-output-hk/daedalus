// The ledger rule violated by a transaction that withdraws rewards from a
// reward account with no vote delegation. The wallet reports it inside the
// message of a `created_invalid_transaction` answer.
export const LEDGER_REFUSES_WITHDRAWAL = 'ConwayWdrlNotDelegatedToDRep';

export type DefiniteSubmissionFailure =
  | 'withdrawal_not_possible_without_vote'
  | 'created_invalid_transaction'
  | 'dapp_submission_failed';

const asRecord = (value: unknown): Record<string, unknown> | null =>
  value != null && typeof value === 'object'
    ? (value as Record<string, unknown>)
    : null;

// The wallet's answer is the error itself (the raw dApp backend error) or the
// `values` of an ApiError or GenericApiError that wraps it.
const walletAnswer = (
  error: unknown
): { code?: unknown; message?: unknown } | null => {
  const record = asRecord(error);
  if (record == null) return null;
  const values = asRecord(record.values);
  return values != null && typeof values.code === 'string' ? values : record;
};

/**
 * Tells whether a transaction submission that was already under way failed in
 * a way the wallet reports as final, as opposed to an outcome that cannot be
 * determined. Returns the code to show, or null when the outcome is unknown.
 */
export const definiteSubmissionFailure = (
  error: unknown
): DefiniteSubmissionFailure | null => {
  const answer = walletAnswer(error);
  if (answer == null) return null;
  const { code, message } = answer;
  if (code === 'created_invalid_transaction') {
    return typeof message === 'string' &&
      message.includes(LEDGER_REFUSES_WITHDRAWAL)
      ? 'withdrawal_not_possible_without_vote'
      : 'created_invalid_transaction';
  }
  if (code === 'dapp_submission_failed') return 'dapp_submission_failed';
  return null;
};
