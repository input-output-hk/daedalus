import BigNumber from 'bignumber.js';
import { balancesFor, decideWithdrawal } from './rewardWithdrawal';
import type { WithdrawalDecisionInput } from './rewardWithdrawal';
import type { LedgerVoteDelegation } from '../../api/wallets/types';

const DREP = {
  kind: 'drep' as const,
  drep: {} as never,
  source: 'ledger' as const,
};
const unknown: LedgerVoteDelegation = { status: 'unknown' };
const notDelegated: LedgerVoteDelegation = { status: 'not_delegated' };
const delegatedDRep: LedgerVoteDelegation = {
  status: 'delegated',
  voting: DREP,
};
const abstain: LedgerVoteDelegation = {
  status: 'delegated',
  voting: { kind: 'abstain' },
};
const noConfidence: LedgerVoteDelegation = {
  status: 'delegated',
  voting: { kind: 'no_confidence' },
};

const input = (
  overrides: Partial<WithdrawalDecisionInput> = {}
): WithdrawalDecisionInput => ({
  purpose: 'payment',
  isLegacy: false,
  ledgerVote: unknown,
  reward: new BigNumber(5000000),
  ...overrides,
});

const allLedgerValues: Array<[string, LedgerVoteDelegation]> = [
  ['unknown', unknown],
  ['not delegated', notDelegated],
  ['a DRep', delegatedDRep],
  ['Abstain', abstain],
  ['No Confidence', noConfidence],
];

describe('decideWithdrawal', () => {
  it.each(['payment', 'undelegation'] as const)(
    'omits for a legacy wallet in any state (%s)',
    (purpose) => {
      allLedgerValues.forEach(([, ledgerVote]) => {
        [true, false, undefined].forEach((withdrawRewards) => {
          expect(
            decideWithdrawal(
              input({ purpose, isLegacy: true, ledgerVote, withdrawRewards })
            )
          ).toEqual({
            kind: 'omit',
            withdrawal: null,
            reason: 'legacy_wallet',
          });
        });
      });
    }
  );

  it('omits a payment from held rewards whatever the user chose', () => {
    [true, false, undefined].forEach((withdrawRewards) => {
      expect(
        decideWithdrawal(input({ ledgerVote: notDelegated, withdrawRewards }))
      ).toEqual({
        kind: 'omit',
        withdrawal: null,
        reason: 'ledger_not_delegated',
      });
    });
  });

  it('asks for a vote delegation first on undelegation from held rewards', () => {
    [true, false, undefined].forEach((withdrawRewards) => {
      expect(
        decideWithdrawal(
          input({
            purpose: 'undelegation',
            ledgerVote: notDelegated,
            withdrawRewards,
          })
        )
      ).toEqual({
        kind: 'delegate_vote_first',
        withdrawal: null,
        reason: 'ledger_not_delegated',
      });
    });
  });

  it.each(['payment', 'undelegation'] as const)(
    'omits at a zero reward for every ledger value (%s)',
    (purpose) => {
      allLedgerValues.forEach(([, ledgerVote]) => {
        expect(
          decideWithdrawal(
            input({ purpose, ledgerVote, reward: new BigNumber(0) })
          )
        ).toEqual({ kind: 'omit', withdrawal: null, reason: 'no_reward' });
      });
    }
  );

  it.each([
    ['unknown', unknown],
    ['a DRep', delegatedDRep],
    ['Abstain', abstain],
    ['No Confidence', noConfidence],
  ])(
    'omits a declined payment when the ledger value is %s',
    (_l, ledgerVote) => {
      expect(
        decideWithdrawal(input({ ledgerVote, withdrawRewards: false }))
      ).toEqual({ kind: 'omit', withdrawal: null, reason: 'declined' });
    }
  );

  it('withdraws when the ledger value is unknown', () => {
    [undefined, true].forEach((withdrawRewards) => {
      expect(decideWithdrawal(input({ withdrawRewards }))).toEqual({
        kind: 'withdraw',
        withdrawal: 'self',
        reason: 'ledger_unknown',
      });
    });
  });

  it.each([
    ['a DRep', delegatedDRep],
    ['Abstain', abstain],
    ['No Confidence', noConfidence],
  ])('withdraws when the ledger value is %s', (_label, ledgerVote) => {
    ['payment', 'undelegation'].forEach((purpose) => {
      [undefined, true].forEach((withdrawRewards) => {
        expect(
          decideWithdrawal(
            input({
              purpose: purpose as 'payment' | 'undelegation',
              ledgerVote,
              withdrawRewards,
            })
          )
        ).toEqual({
          kind: 'withdraw',
          withdrawal: 'self',
          reason: 'ledger_delegated',
        });
      });
    });
  });

  it('ignores a declined withdrawal on undelegation', () => {
    allLedgerValues
      .filter(([, ledgerVote]) => ledgerVote.status !== 'not_delegated')
      .forEach(([, ledgerVote]) => {
        expect(
          decideWithdrawal(
            input({
              purpose: 'undelegation',
              ledgerVote,
              withdrawRewards: false,
            })
          )
        ).toMatchObject({ kind: 'withdraw', withdrawal: 'self' });
      });
  });
});

describe('balancesFor', () => {
  const wallet = {
    amount: new BigNumber(100),
    availableAmount: new BigNumber(70),
    reward: new BigNumber(30),
  };
  const plain = (b: ReturnType<typeof balancesFor>) => ({
    walletBalance: b.walletBalance.toString(),
    availableBalance: b.availableBalance.toString(),
    rewardsBalance: b.rewardsBalance.toString(),
  });

  it('counts the reward as sendable when the request withdraws it', () => {
    expect(plain(balancesFor(wallet, 'self'))).toEqual({
      walletBalance: '100',
      availableBalance: '100',
      rewardsBalance: '30',
    });
  });

  it('takes the reward out of both balances when the request omits it', () => {
    expect(plain(balancesFor(wallet, null))).toEqual({
      walletBalance: '70',
      availableBalance: '70',
      rewardsBalance: '0',
    });
  });

  it('leaves a wallet with no reward unchanged either way', () => {
    const free = {
      ...wallet,
      amount: new BigNumber(70),
      reward: new BigNumber(0),
    };
    expect(plain(balancesFor(free, null))).toEqual(
      plain(balancesFor(free, 'self'))
    );
  });

  it('keeps the wallet balance above the available balance only by locked funds', () => {
    const pending = { ...wallet, amount: new BigNumber(120) };
    const omitted = balancesFor(pending, null);
    expect(omitted.walletBalance.gt(omitted.availableBalance)).toBe(true);
    const idle = balancesFor(wallet, null);
    expect(idle.walletBalance.gt(idle.availableBalance)).toBe(false);
  });

  it('reports the held reward as held when the request omits it', () => {
    const held = { ...wallet, heldReward: new BigNumber(30) };
    expect(balancesFor(held, null).heldRewardsBalance?.toString()).toBe('30');
  });

  it('reports no held reward when the request withdraws', () => {
    const held = { ...wallet, heldReward: new BigNumber(30) };
    expect(balancesFor(held, 'self').heldRewardsBalance?.toString()).toBe('0');
  });
});
