import BigNumber from 'bignumber.js';
import type { AssetMetadata } from '../../../api/assets/types';
import { formattedTokenWalletAmount } from '../../../utils/formatters';
import type { DiscreetValueReplacer } from '../types';

export type DiscreetWalletTokenAmountProps = {
  amount: BigNumber;
  metadata?: AssetMetadata | null | undefined;
  decimals: number | null | undefined;
  isShort?: boolean;
};
export const discreetWalletTokenAmount: DiscreetValueReplacer = ({
  amount,
  metadata,
  decimals,
  isShort,
}: DiscreetWalletTokenAmountProps) => {
  return (isDiscreetMode, replacement) => {
    if (!isDiscreetMode) {
      return formattedTokenWalletAmount(amount, metadata, decimals, isShort);
    }

    const { ticker } = metadata || {};

    if (!ticker) {
      return replacement;
    }

    return `${replacement} ${ticker}`;
  };
};
