import BigNumber from 'bignumber.js';
import Wallet from './Wallet';
import type { WalletProps } from './Wallet';
import {
  balancesFor,
  decideWithdrawal,
} from '../utils/governance/rewardWithdrawal';
import type { LedgerVoteDelegation } from '../api/wallets/types';

const wallet = (overrides: Partial<WalletProps> = {}) =>
  new Wallet({
    id: 'wallet-a',
    addressPoolGap: 20,
    singleAddressMode: false,
    name: 'Wallet A',
    amount: new BigNumber(100),
    availableAmount: new BigNumber(60),
    reward: new BigNumber(40),
    assets: { available: [], total: [] },
    passwordUpdateDate: null,
    syncState: { status: 'ready' },
    isLegacy: false,
    discovery: 'sequential',
    hasPassword: true,
    ...overrides,
  });

describe('Wallet held rewards', () => {
  it('defaults the ledger vote to unknown', () => {
    expect(wallet().ledgerVote).toEqual({ status: 'unknown' });
    expect(wallet({ ledgerVote: undefined }).ledgerVote).toEqual({
      status: 'unknown',
    });
  });

  it('holds the whole reward while the ledger holds no vote delegation', () => {
    const held = wallet({ ledgerVote: { status: 'not_delegated' } });
    expect(held.heldReward.toString()).toBe('40');
    expect(held.hasHeldRewards).toBe(true);
    expect(held.spendableAmount.toString()).toBe('60');
  });

  it.each([
    ['unknown', { status: 'unknown' }],
    ['Abstain', { status: 'delegated', voting: { kind: 'abstain' } }],
  ] as const)('holds nothing when the ledger value is %s', (_l, ledgerVote) => {
    const free = wallet({ ledgerVote });
    expect(free.heldReward.toString()).toBe('0');
    expect(free.hasHeldRewards).toBe(false);
    expect(free.spendableAmount.toString()).toBe('100');
  });

  it('holds nothing at a zero reward', () => {
    const none = wallet({
      ledgerVote: { status: 'not_delegated' },
      reward: new BigNumber(0),
    });
    expect(none.hasHeldRewards).toBe(false);
    expect(none.spendableAmount.toString()).toBe('100');
  });

  it('holds nothing for a legacy wallet', () => {
    const legacy = wallet({
      isLegacy: true,
      ledgerVote: { status: 'not_delegated' },
    });
    expect(legacy.hasHeldRewards).toBe(false);
    expect(legacy.spendableAmount.toString()).toBe('100');
  });

  it('carries the ledger vote through update', () => {
    const current = wallet();
    current.update(wallet({ ledgerVote: { status: 'not_delegated' } }));
    expect(current.ledgerVote).toEqual({ status: 'not_delegated' });
    expect(current.hasHeldRewards).toBe(true);
    current.update(wallet());
    expect(current.ledgerVote).toEqual({ status: 'unknown' });
  });
});

describe('Wallet funds held by a pending transaction', () => {
  it('is false when the total is the available amount plus the reward', () => {
    expect(wallet().hasFundsHeldByPendingTransaction).toBe(false);
  });

  it('is true when the total exceeds the available amount plus the reward', () => {
    const locked = wallet({
      amount: new BigNumber(110),
      availableAmount: new BigNumber(60),
      reward: new BigNumber(40),
    });
    expect(locked.hasFundsHeldByPendingTransaction).toBe(true);
  });

  it('is true for a wallet with no reward when funds are held', () => {
    const locked = wallet({
      amount: new BigNumber(100),
      availableAmount: new BigNumber(60),
      reward: new BigNumber(0),
    });
    expect(locked.hasFundsHeldByPendingTransaction).toBe(true);
  });

  it('is false for a wallet with no reward and nothing held', () => {
    const free = wallet({
      amount: new BigNumber(100),
      availableAmount: new BigNumber(100),
      reward: new BigNumber(0),
    });
    expect(free.hasFundsHeldByPendingTransaction).toBe(false);
  });
});

describe('Wallet spendableAmount and the amount a request can spend', () => {
  const ledgerValues: Array<[string, LedgerVoteDelegation]> = [
    ['unknown', { status: 'unknown' }],
    ['not delegated', { status: 'not_delegated' }],
    ['a DRep', { status: 'delegated', voting: { kind: 'abstain' } }],
  ];

  it.each(ledgerValues)(
    'agree for a payment with the ledger value %s unless the user declined',
    (_name, ledgerVote) => {
      [
        wallet({ ledgerVote }),
        wallet({ ledgerVote, reward: new BigNumber(0) }),
        // A legacy wallet's reward always parses as zero.
        wallet({ ledgerVote, isLegacy: true, reward: new BigNumber(0) }),
      ].forEach((w) => {
        const decision = decideWithdrawal({
          purpose: 'payment',
          isLegacy: w.isLegacy,
          ledgerVote: w.ledgerVote,
          reward: w.reward,
        });
        expect(
          balancesFor(w, decision.withdrawal).walletBalance.toString()
        ).toBe(w.spendableAmount.toString());
      });
    }
  );

  it.each(ledgerValues)(
    'take the reward out of the request amount for a declined payment, and out of spendableAmount only when the ledger holds it, with the ledger value %s',
    (_name, ledgerVote) => {
      const w = wallet({ ledgerVote });
      const decision = decideWithdrawal({
        purpose: 'payment',
        isLegacy: w.isLegacy,
        ledgerVote: w.ledgerVote,
        reward: w.reward,
        withdrawRewards: false,
      });
      const spendable = balancesFor(w, decision.withdrawal).walletBalance;
      expect(spendable.toString()).toBe('60');
      // Held rewards are already out of the figure, so only a decision that
      // declines the reward leaves the two apart.
      const declined = decision.reason === 'declined';
      expect(w.spendableAmount.toString()).toBe(declined ? '100' : '60');
    }
  );
});
