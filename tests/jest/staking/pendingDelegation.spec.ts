import { getPendingStakePoolIdForEpoch } from '../../../source/renderer/app/components/staking/delegation-center/pendingDelegation';
import type { WalletPendingDelegations } from '../../../source/renderer/app/api/wallets/types';

const POOL = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2';
const POOL_NEXT = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx7n01m3';
const FALLBACK = 'pool1fallback000000000000000000000000000000000000000000';
const EPOCH = 412;

const at = (epochNumber: number) => ({
  epoch_number: epochNumber,
  epoch_start_time: '2026-08-10T21:44:51Z',
});

const pending = (...entries: unknown[]): WalletPendingDelegations =>
  entries as WalletPendingDelegations;

describe('getPendingStakePoolIdForEpoch', () => {
  it('falls back when there are no pending delegations', () => {
    expect(getPendingStakePoolIdForEpoch(null, EPOCH, FALLBACK)).toBe(FALLBACK);
    expect(getPendingStakePoolIdForEpoch(pending(), EPOCH, FALLBACK)).toBe(
      FALLBACK
    );
  });

  it('returns the pool of a pending stake delegation', () => {
    const result = getPendingStakePoolIdForEpoch(
      pending({
        status: 'delegating',
        target: POOL_NEXT,
        changes_at: at(EPOCH),
      }),
      EPOCH,
      FALLBACK
    );
    expect(result).toBe(POOL_NEXT);
  });

  it('falls back when a vote delegation is the only pending change', () => {
    // PR #3355: reading whichever entry matched the epoch first reported the
    // wallet as not delegating, though its stake delegation was untouched.
    const result = getPendingStakePoolIdForEpoch(
      pending({ status: 'voting', changes_at: at(EPOCH) }),
      EPOCH,
      FALLBACK
    );
    expect(result).toBe(FALLBACK);
  });

  it('finds the stake entry when a vote entry shares the epoch, in either order', () => {
    const stake = {
      status: 'delegating',
      target: POOL_NEXT,
      changes_at: at(EPOCH),
    };
    const vote = { status: 'voting', changes_at: at(EPOCH) };

    expect(
      getPendingStakePoolIdForEpoch(pending(vote, stake), EPOCH, FALLBACK)
    ).toBe(POOL_NEXT);
    expect(
      getPendingStakePoolIdForEpoch(pending(stake, vote), EPOCH, FALLBACK)
    ).toBe(POOL_NEXT);
  });

  it('returns the pool of a voting_and_delegating entry', () => {
    const result = getPendingStakePoolIdForEpoch(
      pending({
        status: 'voting_and_delegating',
        target: POOL_NEXT,
        changes_at: at(EPOCH),
      }),
      EPOCH,
      FALLBACK
    );
    expect(result).toBe(POOL_NEXT);
  });

  it('reports no pool for a pending undelegation', () => {
    const result = getPendingStakePoolIdForEpoch(
      pending({ status: 'not_delegating', changes_at: at(EPOCH) }),
      EPOCH,
      FALLBACK
    );
    expect(result).toBeNull();
  });

  it('ignores entries that activate in another epoch', () => {
    const result = getPendingStakePoolIdForEpoch(
      pending({
        status: 'delegating',
        target: POOL,
        changes_at: at(EPOCH + 1),
      }),
      EPOCH,
      FALLBACK
    );
    expect(result).toBe(FALLBACK);
  });
});
