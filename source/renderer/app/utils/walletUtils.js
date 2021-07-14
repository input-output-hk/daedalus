// @flow
import BigNumber from 'bignumber.js';

// $FlowFixMe TODO: fix this in flowconfig
export default import('@iohk-jormungandr/wallet-js').then((modules) => modules);

const MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS: number = 10;

export const isWalletRewardsWithdrawalPossible = (
  transactionAmount: BigNumber,
  walletBalance: BigNumber
): boolean =>
  transactionAmount.isGreaterThan(
    walletBalance.minus(MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS)
  );
