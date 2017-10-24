// @flow
import BigNumber from 'bignumber.js';

export const formattedWalletAmount = (amount: BigNumber) => (
  amount.toFormat(6)
);
