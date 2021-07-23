// @flow
import BigNumber from 'bignumber.js';
import Wallet from '../domains/Wallet';

// $FlowFixMe TODO: fix this in flowconfig
export default import('@iohk-jormungandr/wallet-js').then((modules) => modules);

const MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS: number = 10; // 1 ADA | unit: ADA

export const isWalletRewardsWithdrawalPossible = (
  transactionAmount: BigNumber,
  walletBalance: BigNumber
): boolean =>
  !!transactionAmount &&
  !!walletBalance &&
  transactionAmount
    .plus(MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS)
    .isLessThanOrEqualTo(walletBalance);

// For more details check acceptance tests https://github.com/input-output-hk/daedalus/pull/2617
export const shouldShowEmptyWalletWarning = (
  totalAmountToSpend: BigNumber,
  wallet: Wallet,
  hasAssets: boolean = false
): boolean => {
  const { amount: walletBalance, isLegacy, isDelegating } = wallet;
  const hasZeroBalanceAndIsNotDelegating =
    !isDelegating &&
    walletBalance.minus(totalAmountToSpend).isZero() &&
    !hasAssets;
  return (
    !hasZeroBalanceAndIsNotDelegating &&
    !isLegacy &&
    !isWalletRewardsWithdrawalPossible(totalAmountToSpend, walletBalance)
  );
};
