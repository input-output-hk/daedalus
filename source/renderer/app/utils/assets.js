// @flow
import find from 'lodash/find';
import type { Token, Tokens, AssetToken } from '../api/assets/types';
import { TransactionTypes } from '../domains/WalletTransaction';
import type { TransactionType } from '../api/transactions/types';

/**
 *
 * This function removes the `change` assets
 * that are included in the TX API response
 *
 */
export const filterAssets = (
  assets: Array<any>,
  transactionType: TransactionType,
  isInternalAddress: Function
): Array<any> =>
  assets.filter(
    ({ address }) =>
      (transactionType === TransactionTypes.INCOME &&
        isInternalAddress(address)) ||
      (transactionType === TransactionTypes.EXPEND &&
        !isInternalAddress(address))
  );

/**
 *
 * This function receives a Token (the asset included in a wallet or transaction)
 * and combines with the data from the Asset
 *
 * Data from the Token: policyId, assetName, quantity, address
 * Data from the Asset: fingerprint, metadata, decimals, recommendedDecimals, uniqueId
 */
export const getAssetToken = (asset: Token, getAsset: Function): AssetToken => {
  const { policyId, assetName, quantity, address } = asset;
  const { fingerprint, metadata, decimals, recommendedDecimals, uniqueId } =
    getAsset(policyId, assetName) || {};
  return {
    policyId,
    assetName,
    quantity,
    address,
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    uniqueId,
  };
};

/**
 *
 * This function receives a list of Tokens (the assets included in a wallet or transaction)
 * then retrieves the Assets
 * and sort them accordingly
 *
 */
export const getAssetTokens = (
  tokens: Tokens,
  getAsset: Function
): Array<AssetToken> =>
  tokens
    .map((token) => getAssetToken(token, getAsset))
    .filter((token) => !!token.uniqueId)
    .sort(sortAssets);

export const sortAssets = (asset1: AssetToken, asset2: AssetToken) => {
  if (asset1 && asset2) {
    if (asset1.fingerprint < asset2.fingerprint) {
      return -1;
    }
    if (asset1.fingerprint > asset2.fingerprint) {
      return 1;
    }
  }
  return 0;
};

export const hasTokenLeftAfterTransaction = (
  availableAssets: AssetToken[],
  selectedAssetsState?: any
): boolean => {
  if (
    !!selectedAssetsState &&
    selectedAssetsState.length &&
    selectedAssetsState.length > 0 &&
    !!availableAssets &&
    availableAssets?.length &&
    availableAssets?.length > 0
  ) {
    if (selectedAssetsState.length !== availableAssets.length) {
      return true;
    }
    return !!find(
      selectedAssetsState,
      (selectedAsset, index) =>
        !availableAssets[index]?.quantity?.isEqualTo(selectedAsset)
    );
  }
  return false;
};
