// @flow
import find from 'lodash/find';
import BigNumber from 'bignumber.js';
import { filter, escapeRegExp } from 'lodash';
import Wallet from '../domains/Wallet';
import Asset from '../domains/Asset';
import type { Token, Tokens, AssetToken } from '../api/assets/types';
import { TransactionTypes } from '../domains/WalletTransaction';
import type { TransactionType } from '../api/transactions/types';

export type SortBy = 'token' | 'fingerprint' | 'quantity';
export type SortDirection = 'asc' | 'desc';

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

export const getZeroToken = ({
  policyId,
  assetName,
  uniqueId,
}: Asset): Token => ({
  policyId,
  assetName,
  uniqueId,
  quantity: new BigNumber(0),
});

export const getToken = (asset: Asset, tokens: Tokens) => {
  let token = tokens.find(({ uniqueId }) => uniqueId === asset.uniqueId);
  if (!token) {
    token = getZeroToken(asset);
  }
  return token;
};

/**
 *
 * This function receives a Token (the asset included in a wallet or transaction)
 * and combines with the data from the Asset
 *
 * Data from the Token: policyId, assetName, quantity, address
 * Data from the Asset: fingerprint, metadata, decimals, recommendedDecimals, uniqueId
 */
export const getAssetToken = (
  { fingerprint, metadata, decimals, recommendedDecimals, uniqueId }: Asset,
  { policyId, assetName, quantity, address }: Token
): AssetToken => ({
  policyId,
  assetName,
  quantity,
  address,
  fingerprint,
  metadata,
  decimals,
  recommendedDecimals,
  uniqueId,
});

/**
 *
 * This function receives a list of Assets
 * then retrieves the Token from that specific wallet
 * and sort them accordingly
 *
 */
export const getAssetTokens = (
  assets: Array<Asset>,
  tokens: Tokens
): Array<AssetToken> =>
  assets
    .map((asset) => getAssetToken(asset, getToken(asset, tokens)))
    .filter((token) => !!token.uniqueId)
    .sort(sortAssets('fingerprint', 'asc'));

/**
 *
 * This function receives a Token (the asset included in a wallet or transaction)
 * and combines with the data from the Asset
 *
 * Data from the Token: policyId, assetName, quantity, address
 * Data from the Asset: fingerprint, metadata, decimals, recommendedDecimals, uniqueId
 */
export const getNonZeroAssetToken = (
  asset: Token,
  getAsset: Function
): AssetToken => {
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
export const getNonZeroAssetTokens = (
  tokens: Tokens,
  getAsset: Function
): Array<AssetToken> =>
  tokens
    .map((token) => getNonZeroAssetToken(token, getAsset))
    .filter((token) => !!token.uniqueId)
    .sort(sortAssets('fingerprint', 'asc'));

export const sortAssets = (sortBy: SortBy, sortDirection: SortDirection) => (
  asset1: AssetToken,
  asset2: AssetToken
) => {
  const {
    quantity: quantity1,
    fingerprint: fingerprint1,
    metadata: metadata1,
  } = asset1;
  const { name: name1 } = metadata1 || {};
  const {
    quantity: quantity2,
    fingerprint: fingerprint2,
    metadata: metadata2,
  } = asset2;
  const { name: name2 } = metadata2 || {};
  if (sortBy === 'token') {
    if (name1 && !name2) return -1;
    if (!name1 && name2) return 1;
    if (name1 && name2) {
      if (sortDirection === 'asc') {
        return name1.localeCompare(name2);
      }
      return name2.localeCompare(name1);
    }

    if (sortDirection === 'asc') {
      return fingerprint1.localeCompare(fingerprint2);
    }
    return fingerprint2.localeCompare(fingerprint1);
  }
  if (sortBy === 'fingerprint') {
    if (sortDirection === 'asc') {
      return fingerprint1.localeCompare(fingerprint2);
    }
    return fingerprint2.localeCompare(fingerprint1);
  }
  if (sortBy === 'quantity') {
    if (sortDirection === 'asc') {
      return quantity1.isLessThan(quantity2) ? -1 : 1;
    }
    return quantity1.isLessThan(quantity2) ? 1 : -1;
  }
  return 0;
};

/**
 * Check if after the transactions your wallet has some assets left
 *
 * @param allAvailableTokens Collection of assets in your wallet
 * @param initialSelectedAssets Collection of assets initially preselected
 * @param selectedAssets Selected assets to be send in the transaction
 * @returns {boolean}
 */
export const hasTokensLeftAfterTransaction = (
  allAvailableTokens: AssetToken[],
  initialSelectedAssets: AssetToken[],
  selectedAssets?: string[]
): boolean => {
  if (
    !!selectedAssets &&
    selectedAssets.length &&
    selectedAssets.length > 0 &&
    !!initialSelectedAssets &&
    initialSelectedAssets?.length &&
    initialSelectedAssets?.length > 0
  ) {
    // If there is a minimal difference between the assets selected and the
    // ones available in your wallet means you left assets in your wallet
    if (
      initialSelectedAssets.length < allAvailableTokens.length ||
      selectedAssets.length < initialSelectedAssets.length
    ) {
      return true;
    }
    return !!find(
      selectedAssets,
      (selectedAsset, index) =>
        !initialSelectedAssets[index]?.quantity?.isEqualTo(selectedAsset)
    );
  }
  return false;
};

/**
 * Generic function for filtering AssetTokens
 */
export const searchAssets = (
  rawSearchValue: string,
  assets: Array<AssetToken>
) => {
  const searchValue = rawSearchValue.trim();
  if (searchValue.length < 3) {
    return assets;
  }
  return filter(assets, (asset) => {
    const { policyId, assetName, fingerprint, metadata } = asset;
    const { name, ticker, description } = metadata || {};
    const checkList = [
      policyId,
      assetName,
      fingerprint,
      metadata,
      name,
      ticker,
      description,
    ];
    const regex = new RegExp(escapeRegExp(searchValue), 'i');
    return checkList.some((item) => regex.test(item));
  });
};

export const isTokenMissingInWallet = (wallet?: ?Wallet, token?: Token) => {
  if (!wallet || !token || !token.uniqueId) {
    return false;
  }
  const { available } = wallet.assets;
  const { uniqueId } = token;
  return !available.find((walletToken) => walletToken.uniqueId === uniqueId);
};

export const tokenHasBalance = (token: Token, amount: BigNumber) =>
  token.quantity.isGreaterThanOrEqualTo(amount);
