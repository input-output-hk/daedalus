import BigNumber from 'bignumber.js';
import type { LedgerVoteDelegation } from '../../api/wallets/types';
import type { Balances } from '../../api/errors';

export type WithdrawalPurpose = 'payment' | 'undelegation';

export type WithdrawalDecisionInput = Readonly<{
  purpose: WithdrawalPurpose;
  isLegacy: boolean;
  ledgerVote: LedgerVoteDelegation;
  reward: BigNumber;
  // The user's choice for this transaction. Absent means withdraw. Ignored for
  // undelegation, which always withdraws.
  withdrawRewards?: boolean;
}>;

export type WithdrawalDecision =
  | Readonly<{
      kind: 'withdraw';
      withdrawal: 'self';
      reason: 'ledger_unknown' | 'ledger_delegated';
    }>
  | Readonly<{
      kind: 'omit';
      withdrawal: null;
      reason:
        | 'legacy_wallet'
        | 'ledger_not_delegated'
        | 'no_reward'
        | 'declined';
    }>
  | Readonly<{
      kind: 'delegate_vote_first';
      withdrawal: null;
      reason: 'ledger_not_delegated';
    }>;

/**
 * Decides whether a transaction request asks the wallet to withdraw the
 * account's rewards. The ledger refuses a withdrawal from an account that
 * holds no vote delegation, so such an account never withdraws: a payment
 * omits the withdrawal, and an undelegation needs a vote delegation first.
 * The first matching case decides.
 */
export const decideWithdrawal = ({
  purpose,
  isLegacy,
  ledgerVote,
  reward,
  withdrawRewards,
}: WithdrawalDecisionInput): WithdrawalDecision => {
  if (isLegacy) {
    return { kind: 'omit', withdrawal: null, reason: 'legacy_wallet' };
  }
  const hasReward = reward != null && reward.gt(0);
  if (ledgerVote.status === 'not_delegated' && hasReward) {
    return purpose === 'undelegation'
      ? {
          kind: 'delegate_vote_first',
          withdrawal: null,
          reason: 'ledger_not_delegated',
        }
      : { kind: 'omit', withdrawal: null, reason: 'ledger_not_delegated' };
  }
  if (!hasReward) {
    return { kind: 'omit', withdrawal: null, reason: 'no_reward' };
  }
  if (purpose === 'payment' && withdrawRewards === false) {
    return { kind: 'omit', withdrawal: null, reason: 'declined' };
  }
  return {
    kind: 'withdraw',
    withdrawal: 'self',
    reason:
      ledgerVote.status === 'unknown' ? 'ledger_unknown' : 'ledger_delegated',
  };
};

/**
 * The balances a request is checked and priced against. A reward the request
 * does not withdraw is not sendable, so it leaves both the wallet balance and
 * the available balance; the three cases of the not-enough-money handler stay
 * consistent because the wallet balance never exceeds the available balance
 * by the reward alone.
 */
export const balancesFor = (
  wallet: Readonly<{
    amount: BigNumber;
    availableAmount: BigNumber;
    reward: BigNumber;
    heldReward?: BigNumber;
  }>,
  withdrawal: 'self' | null
): Balances =>
  withdrawal === 'self'
    ? {
        walletBalance: wallet.amount,
        availableBalance: wallet.availableAmount.plus(wallet.reward),
        rewardsBalance: wallet.reward,
        heldRewardsBalance: new BigNumber(0),
      }
    : {
        walletBalance: wallet.amount.minus(wallet.reward),
        availableBalance: wallet.availableAmount,
        rewardsBalance: new BigNumber(0),
        heldRewardsBalance: wallet.heldReward ?? new BigNumber(0),
      };
