import BigNumber from 'bignumber.js';
import { AssetToken } from '../../../api/assets/types';
import { formattedTokenWalletAmount } from '../../../utils/formatters';

export const getFormattedAssetAmount = (
  { metadata, decimals }: AssetToken,
  assetAmount = 0
) => {
  return formattedTokenWalletAmount(
    new BigNumber(assetAmount),
    metadata,
    decimals
  );
};
