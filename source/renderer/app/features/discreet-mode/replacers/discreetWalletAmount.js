// @flow
import BigNumber from 'bignumber.js';
import { formattedWalletAmount } from '../../../utils/formatters';
import { DiscreetValueReplacer } from '../types';

export type DiscreetWalletAmountProps = {
  amount: BigNumber,
  withCurrency?: boolean,
  long?: boolean,
};

export const discreetWalletAmount: DiscreetValueReplacer = ({
  amount,
  withCurrency = true,
  long = true,
}: DiscreetWalletAmountProps) => {
  return (isDiscreetMode, replacement) => {
    if (!isDiscreetMode) {
      return formattedWalletAmount(amount, withCurrency, long);
    }
    if (!withCurrency) {
      return replacement;
    }
    return `${replacement} ADA`;
  };
};
