import BigNumber from 'bignumber.js';
import { handleNotEnoughMoneyError, messages } from './errors';
import { balancesFor } from '../utils/governance/rewardWithdrawal';

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const lovelace = (value: number) => new BigNumber(value);

// Wallet amounts follow the wallet API: the total includes the reward and
// anything locked by pending transactions, the available amount excludes both.
const wallet = (amount: number, availableAmount: number, reward: number) => ({
  amount: lovelace(amount),
  availableAmount: lovelace(availableAmount),
  reward: lovelace(reward),
});

const failureId = (balances: ReturnType<typeof balancesFor>): string => {
  try {
    handleNotEnoughMoneyError({ code: 'not_enough_money' }, balances);
  } catch (error) {
    return (error as { id: string }).id;
  }
  throw new Error('handleNotEnoughMoneyError did not throw');
};

const idOf = (key: keyof typeof messages) => messages[key].id;

describe('handleNotEnoughMoneyError', () => {
  describe('with the balances of a request that omits the withdrawal', () => {
    it('reports not enough funds for a held-rewards wallet with no other funds', () => {
      const held = wallet(5000, 0, 5000);
      expect(failureId(balancesFor(held, null))).toBe(
        idOf('notEnoughFundsForTransaction')
      );
    });

    it('reports not enough funds for a held-rewards wallet with other funds and nothing pending', () => {
      const held = wallet(15000, 10000, 5000);
      expect(failureId(balancesFor(held, null))).toBe(
        idOf('notEnoughFundsForTransaction')
      );
    });

    it('still reports pending transactions when funds are locked', () => {
      const held = wallet(20000, 10000, 5000);
      expect(failureId(balancesFor(held, null))).toBe(
        idOf('canNotCalculateTransactionFees')
      );
    });

    it('reports not enough funds for a delegated wallet with no reward', () => {
      const delegated = wallet(10000, 10000, 0);
      expect(failureId(balancesFor(delegated, null))).toBe(
        idOf('notEnoughFundsForTransaction')
      );
    });
  });

  describe('with the balances of a request that withdraws', () => {
    it('reports pending transactions when funds are locked', () => {
      const delegated = wallet(20000, 10000, 5000);
      expect(failureId(balancesFor(delegated, 'self'))).toBe(
        idOf('canNotCalculateTransactionFees')
      );
    });

    it('reports depleted inputs when the wallet holds only rewards', () => {
      const onlyRewards = wallet(5000, 0, 5000);
      expect(failureId(balancesFor(onlyRewards, 'self'))).toBe(
        idOf('inputsDepleted')
      );
    });

    it('reports not enough funds otherwise', () => {
      const delegated = wallet(15000, 10000, 5000);
      expect(failureId(balancesFor(delegated, 'self'))).toBe(
        idOf('notEnoughFundsForTransaction')
      );
    });
  });
});
