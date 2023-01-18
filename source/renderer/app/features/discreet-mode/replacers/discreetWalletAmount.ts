import BigNumber from 'bignumber.js';
import { formattedWalletAmount } from '../../../utils/formatters';
import type { DiscreetValueReplacer } from '../types';

export type DiscreetWalletAmountProps = {
  amount: BigNumber;
  currency?: string;
  withCurrency?: boolean;
  long?: boolean;
};
export const discreetWalletAmount: DiscreetValueReplacer = ({
  amount,
  withCurrency = true,
  long = true,
  currency = 'ADA',
}: DiscreetWalletAmountProps) => {
  return (isDiscreetMode, replacement) => {
    if (!isDiscreetMode) {
      return formattedWalletAmount(amount, withCurrency, long, currency);
    }

    if (!withCurrency) {
      return replacement;
    }

    return `${replacement} ${currency}`;
  };
};
