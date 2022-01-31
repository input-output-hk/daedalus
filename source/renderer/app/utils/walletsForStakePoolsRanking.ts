import BigNumber from 'bignumber.js';
import Wallet from '../domains/Wallet';
import { MIN_DELEGATION_FUNDS } from '../config/stakingConfig';

export const getFilteredWallets = (wallets: Array<Wallet>): Array<Wallet> => {
  return wallets.filter(
    (w: Wallet) =>
      w.amount.isGreaterThanOrEqualTo(new BigNumber(MIN_DELEGATION_FUNDS)) &&
      !w.isLegacy
  );
};
export const getAllAmounts = (wallets: Array<Wallet>): BigNumber => {
  const filteredWallets = getFilteredWallets(wallets);

  if (filteredWallets.length > 0) {
    return filteredWallets
      .map((w: Wallet) => w.amount)
      .reduce(
        (acc: BigNumber, cur: BigNumber) => acc.plus(cur),
        new BigNumber(0)
      );
  }

  return new BigNumber(MIN_DELEGATION_FUNDS);
};
