// @flow
import find from 'lodash/find';
import BigNumber from 'bignumber.js';
import { filter, escapeRegExp } from 'lodash';
import Wallet from '../domains/Wallet';
import Asset from '../domains/Asset';
import type { Token, Tokens, AssetToken } from '../api/assets/types';
import { TransactionTypes } from '../domains/WalletTransaction';
import type { TransactionType } from '../api/transactions/types';
import { formattedTokenDecimals } from './formatters';

export type SortBy = 'token' | 'fingerprint' | 'quantity';
export type SortDirection = 'asc' | 'desc';

/**
 * A few functions here use Assets and Tokens, so here is a brief difference:
 * Asset - has the asset details (fingerprint, metadata, decimals, recommendedDecimals)
 * Token - has a transaction's details (quantity, address)
 * Both have policyId and assetName
 */

/**
 * Removes the `change` assets
 * that are included in the TX API response
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

/**
 * Receives an asset and a list of tokens
 * Then retrieves the token with the same uniqueId
 * @param asset - asset details
 * @param tokens - list of Tokens
 * See Asset/Token differences at the begining of this doc
 */
export const getToken = (asset: Asset, tokens: Tokens) => {
  let token = tokens.find(({ uniqueId }) => uniqueId === asset.uniqueId);
  if (!token) {
    token = getZeroToken(asset);
  }
  return token;
};

/**
 * Receives a Token and an Asset
 * then merges them into an AssetToken
 * @param asset - asset details
 * @param token - token details
 * See Asset/Token differences at the begining of this doc
 */
export const getAssetToken = (
  {
    policyId,
    assetName,
    assetNameASCII,
    fingerprint,
    metadata,
    decimals,
    recommendedDecimals,
    uniqueId,
  }: Asset,
  { quantity, address }: Token
): AssetToken => ({
  policyId,
  assetName,
  assetNameASCII,
  quantity,
  address,
  fingerprint,
  metadata,
  decimals,
  recommendedDecimals,
  uniqueId,
});

/**
 * Receives both the Assets and the Tokens from a wallet
 * then merges them into AssetTokens
 * @param assets - list of asset details
 * @param tokens - list of token details
 * See Asset/Token differences at the begining of this doc
 */
export const getAssetTokens = (
  assets: Array<Asset>,
  tokens: Tokens
): Array<AssetToken> =>
  assets
    .map((asset) => getAssetToken(asset, getToken(asset, tokens)))
    .filter((token) => !!token.uniqueId)
    // @TOKEN TODO - Remove this filter once we can list zero tokens
    .filter((token) => !token.quantity.isZero());

/**
 * Receives a Token
 * and combines with the data from the Asset
 * @param asset - asset details
 * @param getAsset - function that returns an asset
 * See Asset/Token differences at the begining of this doc
 */
export const getAssetTokenFromToken = (
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

export const getNonZeroAssetTokens = (
  tokens: Tokens,
  getAsset: Function
): Array<AssetToken> =>
  tokens
    .map((token) => getAssetTokenFromToken(token, getAsset))
    .filter((token) => !!token.uniqueId)
    .sort(sortAssets('fingerprint', 'asc'));

/**
 * High-order function for sorting assetTokens
 * @param sortBy - sorting parameter
 * @param sortDirection - should it sort in ascending or descending direction
 */
export const sortAssets = (sortBy: SortBy, sortDirection: SortDirection) => (
  asset1: AssetToken,
  asset2: AssetToken
) => {
  const {
    quantity: unformattedQuantity1,
    fingerprint: fingerprint1,
    metadata: metadata1,
    decimals: decimals1,
  } = asset1;
  const quantity1 = formattedTokenDecimals(unformattedQuantity1, decimals1);
  const { name: name1 } = metadata1 || {};
  const {
    quantity: unformattedQuantity2,
    fingerprint: fingerprint2,
    metadata: metadata2,
    decimals: decimals2,
  } = asset2;
  const quantity2 = formattedTokenDecimals(unformattedQuantity2, decimals2);
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
 * @param rawSearchValue - search value
 * @param assets - AssetTokens to operate the search
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
    const {
      policyId,
      assetName,
      assetNameASCII,
      fingerprint,
      metadata,
    } = asset;
    const { name, ticker, description } = metadata || {};
    const checkList = [
      policyId,
      assetName,
      assetNameASCII,
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

export const getUniqueId = ({
  assetName,
  policyId,
}: {
  assetName: string,
  policyId: string,
}) => `${assetName}${policyId}`;
