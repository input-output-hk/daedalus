// @flow
import BigNumber from 'bignumber.js';
import Wallet from '../domains/Wallet';

// $FlowFixMe TODO: fix this in flowconfig
export default import('@iohk-jormungandr/wallet-js').then((modules) => modules);

const MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS: number = 10; // 1 ADA | unit: ADA

const isWalletRewardsWithdrawalPossible = (
  transactionAmount: BigNumber,
  walletBalance: BigNumber
): boolean =>
  transactionAmount.isGreaterThan(
    walletBalance.minus(MINIMUM_ADA_BALANCE_FOR_WITHDRAWING_REWARDS)
  );

// For more details check acceptance tests https://github.com/input-output-hk/daedalus/pull/2617
export const shouldShowEmptyWalletWarning = (
  totalAmount: BigNumber,
  wallet: Wallet
): boolean => {
  const { amount, isLegacy, isDelegating } = wallet;
  const isNotDelegatingAndZeroBalance =
    isDelegating && !amount.isGreaterThan(0);
  return (
    !isNotDelegatingAndZeroBalance &&
    !isLegacy &&
    isWalletRewardsWithdrawalPossible(totalAmount, amount)
  );
};
