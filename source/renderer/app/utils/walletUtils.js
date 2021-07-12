// @flow
import BigNumber from 'bignumber.js';

// $FlowFixMe TODO: fix this in flowconfig
export default import('@iohk-jormungandr/wallet-js').then((modules) => modules);

const MINIMUM_BALANCE_FOR_REWARD: number = 2;

export const isWalletEmptyWitoutRewards = (
  totalAmount: BigNumber,
  available: BigNumber
): boolean =>
  totalAmount.isGreaterThan(available.minus(MINIMUM_BALANCE_FOR_REWARD));
